##############CONFIDENCE AS A PRIORITY SIGNAL - DATA MANIPULATIONS FOR THE REPLICATION OF EXPERIMENT 1 (WITH CONFIDENCE RATINGS)###################

####ARRANGE DATA AND CREATE USEFUL VARIABLES FOR MAIN DATA FRAME####
total_elements <- 80
exprep1_raw <- exprep1_raw %>% filter(sub!=0)
exprep1_raw <- exprep1_raw %>% filter(run!=0) #xxx to include participants who have less trials, which are participants 74.8751 , 78.0678 , 80.1227 , 82.2378
exprep1_raw <- exprep1_raw %>% arrange(sub,run,block,trial)
exprep1_raw <- exprep1_raw %>% group_by(sub) %>% mutate(
  experf_col = ifelse(number_blues==min(number_blues),90,ifelse(total_elements==number_blues+min(number_blues),90,60)),
  experf_let=ifelse(number_os==min(number_os),90,ifelse(total_elements==number_os+min(number_os),90,60)),
  choice1bin = ifelse(choice1<3,1,0),
  catperf = ifelse(experf_col==60 & experf_let==60,"60-60",ifelse(experf_col==90 & experf_let==60,"90-60",ifelse(experf_col==60 & experf_let==90,"60-90","90-90"))),
  expac = ifelse(catperf=="60-90",-1,ifelse(catperf=="90-60",1,0)),
  expac_lag = lag(expac),
  conffirst = ifelse(choice1<3,hconf,vconf),
  confsecond = ifelse(choice1<3,vconf,hconf))

####COMPARE EXPECTED AND OBSERVED PERFORMANCE####

#individual subjects, averaged runs
exprep1_perfcol_sin_rav <- exprep1_raw %>% group_by(sub,experf_col) %>% do(calculate_proportions(.,correctcol)) %>% mutate(task="color") %>% ungroup()
names(exprep1_perfcol_sin_rav)[2] <- "experf"
exprep1_perflet_sin_rav <- exprep1_raw %>% group_by(sub,experf_let) %>% do(calculate_proportions(.,correctlet)) %>% mutate(task="letter") %>% ungroup()
names(exprep1_perflet_sin_rav)[2] <- "experf"
exprep1_perf_sin_rav <- rbind(exprep1_perfcol_sin_rav,exprep1_perflet_sin_rav)

#averaged subjects, averaged runs
exprep1_perf_sav_rav <- exprep1_perf_sin_rav %>% group_by(task,experf) %>% summarize (prob = mean(prob))

exprep1_perf_sav_rav <- exprep1_perf_sin_rav %>% group_by(task,experf) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

####COLOR OR LETTER BIASES####

##individual subjects, individual runs
exprep1_biascol_sin_rin <- exprep1_raw %>% group_by(sub,run,experf_col) %>% summarise(ratioblue=sum(choiceblue==1)/sum((number_blues>total_elements/2)==TRUE)) %>% mutate(task="color")
names(exprep1_biascol_sin_rin) <- c("sub", "run","experf","ratio","task")
exprep1_biaslet_sin_rin <- exprep1_raw %>% group_by(sub,run,experf_let) %>% summarise(ratioo=sum(choiceo==1)/sum((number_os>total_elements/2)==TRUE)) %>% mutate(task="letter")
names(exprep1_biaslet_sin_rin) <- c("sub", "run","experf","ratio","task")
exprep1_bias_sin_rin <- rbind(exprep1_biascol_sin_rin,exprep1_biaslet_sin_rin)

##individual subjects, averaged runs
exprep1_biascol_sin_rav <- exprep1_raw %>% group_by(sub,experf_col) %>% summarise(ratioblue=sum(choiceblue==1)/sum((number_blues>total_elements/2)==TRUE)) %>% mutate(task="color")
names(exprep1_biascol_sin_rav) <- c("sub", "experf","ratio","task")
exprep1_biaslet_sin_rav <- exprep1_raw %>% group_by(sub,experf_let) %>% summarise(ratioo=sum(choiceo==1)/sum((number_os>total_elements/2)==TRUE)) %>% mutate(task="letter")
names(exprep1_biaslet_sin_rav) <- c("sub", "experf","ratio","task")
exprep1_bias_sin_rav <- rbind(exprep1_biascol_sin_rav,exprep1_biaslet_sin_rav)

##averaged subjects, averaged runs
exprep1_bias_sav_rav <- exprep1_bias_sin_rav %>% group_by(experf,task) %>% summarise(ratio = mean(ratio))

####LOCALIZATION BIASES####

#individual subjects, individual runs
exprep1_loc_sin_rin <- exprep1_raw %>% group_by(sub,run) %>% mutate(ntrials=length(trial))  %>% group_by(sub,run,locresp1) %>% summarise(prop=length(trial)/mean(ntrials))

#individual subjects, averaged runs
exprep1_loc_sin_rav <- exprep1_loc_sin_rin %>% group_by(sub,locresp1) %>% summarise(prop=mean(prop)) #this is good only if each run has the same number of trials 

#averaged subjects, averaged runs
exprep1_loc_sav_rav <- exprep1_loc_sin_rav %>% group_by(locresp1) %>%
  summarise(sd_prop = sd(prop, na.rm = TRUE),
            prop = mean(prop, na.rm = TRUE),
            n_prop = n()) %>%
  mutate(se_prop = sd_prop / sqrt(n_prop),
         low_ci_prop = prop - qt(1 - (0.05 / 2), n_prop - 1) * se_prop,
         up_ci_prop = prop + qt(1 - (0.05 / 2), n_prop - 1) * se_prop)

###PRIORITY

##individual subjects, individual runs
exprep1_prio_sin_rin <- exprep1_raw %>% group_by(sub,run,catperf) %>% do(calculate_proportions(.,choice1bin))

##averaged subjects, individual runs
exprep1_prio_sav_rin <- exprep1_prio_sin_rin %>% group_by(run,catperf) %>% summarise(prob =mean(prob))

#individual subjects, averaged runs
exprep1_prio_sin_rav <- exprep1_raw %>% group_by(sub,catperf) %>% do(calculate_proportions(.,choice1bin))
exprep1_prio_sin_rav <- exprep1_prio_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))

exprep1_priodif_sin_rav <- dcast(data = exprep1_prio_sin_rav,formula = sub~catperf,fun.aggregate = sum,value.var = "prob")
names(exprep1_priodif_sin_rav) <- c("sub", "hard_hard","hard_easy","easy_hard","easy_easy")
exprep1_priodif_sin_rav <- exprep1_priodif_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4)

exprep1_priodif_sav_rav <- exprep1_priodif_sin_rav %>% group_by() %>%
  summarise(sd_difacc = sd(difacc, na.rm = TRUE),
            difacc = mean(difacc, na.rm = TRUE),
            n_difacc = n()) %>%
  mutate(se_difacc = sd_difacc / sqrt(n_difacc),
         low_ci_difacc = difacc - qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc,
         up_ci_difacc = difacc + qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc)

#same but only for correct trials
exprep1_priocorrect_sin_rav <- exprep1_raw %>% group_by(sub,catperf) %>% filter(correct1 == 1 & correct2 == 1) %>% do(calculate_proportions(.,choice1bin))
exprep1_priodifcorrect_sin_rav <- dcast(data = exprep1_priocorrect_sin_rav,formula = sub~catperf,fun.aggregate = sum,value.var = "prob")
names(exprep1_priodifcorrect_sin_rav) <- c("sub", "hard_hard","hard_easy","easy_hard","easy_easy")
exprep1_priodifcorrect_sin_rav <- exprep1_priodifcorrect_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4)

#same but only for error trials
exprep1_prioerror_sin_rav <- exprep1_raw %>% group_by(sub,catperf) %>% filter(correct1 == 0 & correct2 == 0) %>% do(calculate_proportions(.,choice1bin))
exprep1_priodiferror_sin_rav <- dcast(data = exprep1_prioerror_sin_rav,formula = sub~catperf,fun.aggregate = sum,value.var = "prob")
names(exprep1_priodiferror_sin_rav) <- c("sub", "hard_hard","hard_easy","easy_hard","easy_easy")
exprep1_priodiferror_sin_rav <- exprep1_priodiferror_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4,expnumber=2)

#averaged subjects, averaged runs
exprep1_prio_sav_rav <- exprep1_prio_sin_rav %>% group_by(catperf) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

exprep1_prio_sav_rav <- exprep1_prio_sav_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))

####ACCURACY CHOICE 1 VS CHOICE 2####

#individual subjects, averaged runs
exprep1_acord1_sin_rav <- exprep1_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct1))
exprep1_acord1_sin_rav <- exprep1_acord1_sin_rav %>% mutate(order="first")
exprep1_acord2_sin_rav <- exprep1_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct2))
exprep1_acord2_sin_rav <- exprep1_acord2_sin_rav %>% mutate(order="second")
exprep1_acord_sin_rav <- rbind(exprep1_acord1_sin_rav,exprep1_acord2_sin_rav)

#same, but split by EE and HH
exprep1_acord1split_sin_rav <- exprep1_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub,catperf) %>% do(calculate_proportions(.,correct1))
exprep1_acord1split_sin_rav <- exprep1_acord1split_sin_rav %>% mutate(order="first")
exprep1_acord2split_sin_rav <- exprep1_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub,catperf) %>% do(calculate_proportions(.,correct2))
exprep1_acord2split_sin_rav <- exprep1_acord2split_sin_rav %>% mutate(order="second")
exprep1_acordsplit_sin_rav <- rbind(exprep1_acord1split_sin_rav,exprep1_acord2split_sin_rav)
exprep1_acordsplit_sin_rav <- exprep1_acordsplit_sin_rav %>% select(sub,catperf,prob,order) %>% group_by(sub,catperf) %>% spread(order,prob)
exprep1_acordsplit_sin_rav <- exprep1_acordsplit_sin_rav %>% mutate(difprob = first - second)

#same, only by EE and HH, but this time collapsed
exprep1_acord1splitcollapsed_sin_rav <- exprep1_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct1))
exprep1_acord1splitcollapsed_sin_rav <- exprep1_acord1splitcollapsed_sin_rav %>% mutate(order="first")
exprep1_acord2splitcollapsed_sin_rav <- exprep1_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct2))
exprep1_acord2splitcollapsed_sin_rav <- exprep1_acord2splitcollapsed_sin_rav %>% mutate(order="second")
exprep1_acordsplitcollapsed_sin_rav <- rbind(exprep1_acord1splitcollapsed_sin_rav,exprep1_acord2splitcollapsed_sin_rav)
exprep1_acordsplitcollapsed_sin_rav <- exprep1_acordsplitcollapsed_sin_rav %>% select(sub,prob,order) %>% group_by(sub) %>% spread(order,prob)
exprep1_acordsplitcollapsed_sin_rav <- exprep1_acordsplitcollapsed_sin_rav %>% mutate(difprob = first - second)

#collapssed across all trial types: EE, EH, HE, HH
exprep1_acord1togetherall_sin_rav <- exprep1_raw %>%  group_by(sub) %>% do(calculate_proportions(.,correct1))
exprep1_acord1togetherall_sin_rav <- exprep1_acord1togetherall_sin_rav %>% mutate(order="first")
exprep1_acord2togetherall_sin_rav <- exprep1_raw %>% group_by(sub) %>% do(calculate_proportions(.,correct2))
exprep1_acord2togetherall_sin_rav <- exprep1_acord2togetherall_sin_rav %>% mutate(order="second")
exprep1_acordtogetherall_sin_rav <- rbind(exprep1_acord1togetherall_sin_rav,exprep1_acord2togetherall_sin_rav)
exprep1_acordtogetherall_sin_rav <- exprep1_acordtogetherall_sin_rav %>% select(sub,prob,order) %>% group_by(sub) %>% spread(order,prob)
exprep1_acordtogetherall_sin_rav <- exprep1_acordtogetherall_sin_rav %>% mutate(difprob = first - second)

####CONFIDENCE RATINGS####

#confidence comparison when both tasks have same difficulty and both responses were correct
exprep1_conforder_samediff_correct_sin_rav <- exprep1_raw %>% filter(catperf == "60-60" | catperf == "90-90") %>%
  filter(correct1 == 1 & correct2 == 1) %>%
  melt(measure.vars = c("conffirst", "confsecond")) %>%
  group_by(sub, catperf, variable) %>%
  summarise(conf = mean(value), sd=sd(value), se=sd/sqrt(length(value)), E=qnorm(0.975)*se)

#same but averaged across participants
exprep1_conforder_samediff_correct_sav_rav <- exprep1_conforder_samediff_correct_sin_rav %>% group_by(catperf, variable) %>% summarize (conf = mean(conf))

exprep1_conforder_samediff_correct_sav_rav <- exprep1_conforder_samediff_correct_sin_rav %>% group_by(catperf, variable) %>%
  summarise(sd_conf = sd(conf, na.rm = TRUE),
            conf = mean(conf, na.rm = TRUE),
            n_conf = n()) %>%
  mutate(se_conf = sd_conf / sqrt(n_conf),
         low_ci_conf = conf - qt(1 - (0.05 / 2), n_conf - 1) * se_conf,
         up_ci_conf = conf + qt(1 - (0.05 / 2), n_conf - 1) * se_conf)

#bigger confidence first choice than second, binary
exprep1_raw <- exprep1_raw %>% mutate(conffirsthigher = ifelse(conffirst>confsecond,1,0))

exprep1_confhigher_all_correct_sin_rav <- exprep1_raw %>% group_by(sub) %>% do(calculate_proportions(.,conffirsthigher))
exprep1_confhigher_all_correct_sav_rav <- exprep1_confhigher_all_correct_sin_rav %>% ungroup() %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

exprep1_confhigher_samediff_correct_sin_rav <- exprep1_raw %>% filter(catperf == "60-60" | catperf == "90-90") %>%
  filter(correct1 == 1 & correct2 == 1) %>% group_by(sub,catperf) %>% do(calculate_proportions(.,conffirsthigher))
exprep1_confhigher_samediff_correct_sin_rav <- exprep1_confhigher_samediff_correct_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH","EE"))

exprep1_confhigher_samediffcollapsed_correct_sin_rav <- exprep1_raw %>% filter(catperf == "60-60" | catperf == "90-90") %>%
  filter(correct1 == 1 & correct2 == 1) %>% group_by(sub) %>% do(calculate_proportions(.,conffirsthigher))
exprep1_confhigher_samediffcollapsed_correct_sav_rav <- exprep1_confhigher_samediffcollapsed_correct_sin_rav %>% ungroup() %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

exprep1_confhigher_samediffcollapsed_wrong_sin_rav <- exprep1_raw %>% filter(catperf == "60-60" | catperf == "90-90") %>%
  filter(correct1 == 0 & correct2 == 0) %>% group_by(sub) %>% do(calculate_proportions(.,conffirsthigher))
exprep1_confhigher_samediffcollapsed_wrong_sav_rav <- exprep1_confhigher_samediffcollapsed_wrong_sin_rav %>% ungroup() %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

exprep1_confhigher_alltypes_alldata_sin_rav <- exprep1_raw %>% group_by(sub,catperf) %>% do(calculate_proportions(.,conffirsthigher))
exprep1_confhigher_alltypes_alldata_sav_rav <- exprep1_confhigher_alltypes_alldata_sin_rav %>% group_by(catperf) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

exprep1_confhigher_alltypes_correct_sin_rav <- exprep1_raw %>% filter(correct1 == 1 & correct2 == 1) %>% group_by(sub,catperf) %>% do(calculate_proportions(.,conffirsthigher))
exprep1_confhigher_alltypes_correct_sav_rav <- exprep1_confhigher_alltypes_correct_sin_rav %>% group_by(catperf) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

exprep1_confhigher_alltypes_wrong_sin_rav <- exprep1_raw %>% filter(correct1 == 0 & correct2 == 0) %>% group_by(sub,catperf) %>% do(calculate_proportions(.,conffirsthigher))
exprep1_confhigher_alltypes_wrong_sav_rav <- exprep1_confhigher_alltypes_wrong_sin_rav %>% group_by(catperf) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

###confidence 1st vs 2nd choice###
exprep1_conffise_sin_rav <- exprep1_raw %>% group_by(sub) %>% summarise (conffirst = mean(conffirst), confsecond = mean(confsecond))

###confidence correct choices and errors###
exprep1_raw <- exprep1_raw %>% mutate(task1=ifelse(choice1<3,0,1),
                                task2=ifelse(choice2<3,0,1),
                                dif1=ifelse(task1==0 & experf_col==90,0,ifelse(task1==0 & experf_col==60,1,ifelse(task1==1 & experf_let==90,0,1))),
                                dif2=ifelse(task2==0 & experf_col==90,0,ifelse(task2==0 & experf_col==60,1,ifelse(task2==1 & experf_let==90,0,1)))) #if color, 0; if letter, 1

exprep1_taskrep1 <- exprep1_raw %>% select(sub,run,block,trial,task1,dif1,choice1,correct1,locresp1,conffirst)
names(exprep1_taskrep1) <- c("sub","run","block","trial","task","dif","choice","correct","locresp","conf")
exprep1_taskrep2 <- exprep1_raw %>% select(sub,run,block,trial,task2,dif2,choice2,correct2,locresp2,confsecond)
names(exprep1_taskrep2) <- c("sub","run","block","trial","task","dif","choice","correct","locresp","conf")

exprep1_taskrep <- rbind(exprep1_taskrep1,exprep1_taskrep2)

exprep1_confcorer_sin_rav <- exprep1_taskrep %>% group_by(sub,task,dif,correct) %>% summarise(conf=mean(conf))

exprep1_confcorer_sav_rav <- exprep1_confcorer_sin_rav %>% group_by(task,dif,correct) %>%
  summarise(sd_conf = sd(conf, na.rm = TRUE),
            conf = mean(conf, na.rm = TRUE),
            n_conf = n()) %>%
  mutate(se_conf = sd_conf / sqrt(n_conf),
         low_ci_conf = conf - qt(1 - (0.05 / 2), n_conf - 1) * se_conf,
         up_ci_conf = conf + qt(1 - (0.05 / 2), n_conf - 1) * se_conf)

####PERFORMANCE WHEN CHOOSING EASY FIRST VS HARD FIRST####

#individual subjects, averaged runs
exprep1_difchoice_sin_rav <- exprep1_raw %>% filter(catperf=="60-90" | catperf=="90-60") %>% mutate(cheasy_first = ifelse(catperf=="90-60" & choice1bin == 1, 1, ifelse(catperf=="90-60" & choice1bin == 0, 0, ifelse(catperf=="60-90" & choice1bin == 0, 1, 0)))) #if first chosen task is easy, cheasy_first = 1, otherwise 0

exprep1_difchoice1_sin_rav <- exprep1_difchoice_sin_rav %>% group_by(sub,cheasy_first) %>% do(calculate_proportions(.,correct1))
exprep1_difchoicexprep1_sin_rav <- exprep1_difchoice_sin_rav %>% group_by(sub,cheasy_first) %>% do(calculate_proportions(.,correct2))

exprep1_difchoice1_sin_rav <- exprep1_difchoice1_sin_rav %>% mutate(task_dif = ifelse(cheasy_first == 1, 1,0)) #if task_dif = 1, easy task
exprep1_difchoicexprep1_sin_rav <- exprep1_difchoicexprep1_sin_rav %>% mutate(task_dif = ifelse(cheasy_first == 1, 0,1))

exprep1_difchoice_sin_rav <- rbind(exprep1_difchoice1_sin_rav,exprep1_difchoicexprep1_sin_rav)

#averaged subjects, averaged runs
exprep1_difchoice_sav_rav <- exprep1_difchoice_sin_rav %>% group_by(task_dif,cheasy_first) %>% summarize (prob = mean(prob))

exprep1_difchoice_sav_rav <- exprep1_difchoice_sin_rav %>% group_by(task_dif,cheasy_first) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

####PRIORITIZING HIGH-CONFIDENCE ERRORS OVER LOW-CONFIDENCE CORRECT TRIALS####
exprep1_prierror1 <- exprep1_raw %>% filter(correctcol==0 & correctlet==1 & hconf > vconf) %>% group_by(catperf) %>% summarise(ntrials1 = n(), nhits1 = sum(choice1bin==1), prop1 = nhits1/ntrials1) #when: error color, correct letter, and conf for color higher, calculate proportion times color was prioritized over letter
exprep1_prierror2 <- exprep1_raw %>% filter(correctcol==1 & correctlet==0 & hconf < vconf) %>% group_by(catperf) %>% summarise(ntrials2 = n(), nhits2 = sum(choice1bin==0), prop2 = nhits2/ntrials2) #when: correct color, error letter, and conf for letter higher, calculate proportion times letter was prioritized over letter

exprep1_prierror <- left_join(exprep1_prierror1, exprep1_prierror2)
exprep1_prierror <- exprep1_prierror %>% mutate(ntrials = ntrials1 + ntrials2, nhits = nhits1 + nhits2, prop = nhits/ntrials)

####REVERSAL PRIORITY EFFECT FROM DIFFICULTY FOR ERRORS####
exprep1_priosameacc_sin_rav <- exprep1_raw %>% mutate(acccategory=ifelse(correct1==1 & correct2 == 1,"correct",ifelse(correct1==0 & correct2 == 0,"error","none"))) %>% filter(acccategory != "none") %>% group_by(sub,catperf,acccategory) %>% do(calculate_proportions(.,choice1bin))
exprep1_priosameacc_sin_rav <- exprep1_priosameacc_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))

exprep1_priosameaccdif_sin_rav <- exprep1_priosameacc_sin_rav %>% group_by(sub,acccategory) %>% select(sub,acccategory,catperf,prob) %>% spread(catperf,prob)
names(exprep1_priosameaccdif_sin_rav) <- c("sub", "acccategory", "hard_hard","hard_easy","easy_hard","easy_easy")
exprep1_priosameaccdif_sin_rav <- exprep1_priosameaccdif_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4)

exprep1_priosameaccdif_sav_rav <- exprep1_priosameaccdif_sin_rav %>% group_by(acccategory) %>%
  summarise(sd_difacc = sd(difacc, na.rm = TRUE),
            difacc = mean(difacc, na.rm = TRUE),
            n_difacc = n()) %>%
  mutate(se_difacc = sd_difacc / sqrt(n_difacc),
         low_ci_difacc = difacc - qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc,
         up_ci_difacc = difacc + qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc)