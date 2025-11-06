##############CONFIDENCE AS A PRIORITY SIGNAL - DATA MANIPULATIONS FOR THE SUPPLEMENTARY EXPERIMENT WHERE RESPONSE ORDER IS CONTROLLED###################

####ARRANGE DATA AND CREATE USEFUL VARIABLES FOR MAIN DATA FRAME####
total_elements <- 80
expsupp_raw <- expsupp_raw %>% filter(sub!=0)
expsupp_raw <- expsupp_raw %>% arrange(sub,run,block,trial)
expsupp_raw <- expsupp_raw %>% group_by(sub) %>% mutate(
  experf_col = ifelse(number_blues==min(number_blues),90,ifelse(total_elements==number_blues+min(number_blues),90,60)),
  experf_let=ifelse(number_os==min(number_os),90,ifelse(total_elements==number_os+min(number_os),90,60)),
  choice1bin = ifelse(choice1<3,1,0),
  catperf = ifelse(experf_col==60 & experf_let==60,"60-60",ifelse(experf_col==90 & experf_let==60,"90-60",ifelse(experf_col==60 & experf_let==90,"60-90","90-90"))),
  expac = ifelse(catperf=="60-90",-1,ifelse(catperf=="90-60",1,0)),
  expac_lag = lag(expac),
  respordbin = ifelse(current_respord < 2,0,1)) #if 0, choice boxes presented sequentially; if 1, presented simultaneously


####COMPARE EXPECTED AND OBSERVED PERFORMANCE####

#individual subjects, averaged runs
expsupp_perfcol_sin_rav <- expsupp_raw %>% group_by(sub,experf_col) %>% do(calculate_proportions(.,correctcol)) %>% mutate(task="color") %>% ungroup()
names(expsupp_perfcol_sin_rav)[2] <- "experf"
expsupp_perflet_sin_rav <- expsupp_raw %>% group_by(sub,experf_let) %>% do(calculate_proportions(.,correctlet)) %>% mutate(task="letter") %>% ungroup()
names(expsupp_perflet_sin_rav)[2] <- "experf"
expsupp_perf_sin_rav <- rbind(expsupp_perfcol_sin_rav,expsupp_perflet_sin_rav)

#averaged subjects, averaged runs
expsupp_perf_sav_rav <- expsupp_perf_sin_rav %>% group_by(task,experf) %>% summarize (prob = mean(prob))

expsupp_perf_sav_rav <- expsupp_perf_sin_rav %>% group_by(task,experf) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

####PERFORMANCE WHEN CHOOSING EASY FIRST VS HARD FIRST####

#individual subjects, averaged runs

expsupp_difchoice_sin_rav <- expsupp_raw %>% mutate(cheasy_first = ifelse(catperf=="90-60" & choice1bin == 1, 1, ifelse(catperf=="90-60" & choice1bin == 0, 0, ifelse(catperf=="60-90" & choice1bin == 0, 1, 0)))) #if first chosen task is easy, cheasy_first = 1, otherwise 0

expsupp_difrt_sin_rav <- expsupp_difchoice_sin_rav %>% mutate(rt_total = rtchoice1 + rtchoice2)
expsupp_difrt_sin_rav <- expsupp_difrt_sin_rav %>% group_by (sub,respordbin,cheasy_first) %>% summarise(rt_total = mean(rt_total))

expsupp_difchoice1_sin_rav <- expsupp_difchoice_sin_rav %>% group_by(sub,respordbin,cheasy_first) %>% do(calculate_proportions(.,correct1))
expsupp_difchoice2_sin_rav <- expsupp_difchoice_sin_rav %>% group_by(sub,respordbin,cheasy_first) %>% do(calculate_proportions(.,correct2))

expsupp_difchoice1_sin_rav <- expsupp_difchoice1_sin_rav %>% mutate(task_dif = ifelse(cheasy_first == 1, 1,0)) #if task_dif = 1, easy task
expsupp_difchoice2_sin_rav <- expsupp_difchoice2_sin_rav %>% mutate(task_dif = ifelse(cheasy_first == 1, 0,1))

expsupp_difchoice_sin_rav <- rbind(expsupp_difchoice1_sin_rav,expsupp_difchoice2_sin_rav)

#averaged subjects, averaged runs
expsupp_difchoice_sav_rav <- expsupp_difchoice_sin_rav %>% group_by(respordbin,task_dif,cheasy_first) %>% summarize (prob = mean(prob))

expsupp_difchoice_sav_rav <- expsupp_difchoice_sin_rav %>% group_by(respordbin,task_dif,cheasy_first) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

###PRIORITY

#individual subjects, averaged runs
expsupp_prio_sin_rav <- expsupp_raw %>% filter(respordbin == 1) %>% group_by(sub,catperf) %>% do(calculate_proportions(.,choice1bin))

expsupp_priodif_sin_rav <- dcast(data = expsupp_prio_sin_rav,formula = sub~catperf,fun.aggregate = sum,value.var = "prob")
names(expsupp_priodif_sin_rav) <- c("sub", "hard_easy","easy_hard")
expsupp_priodif_sin_rav <- expsupp_priodif_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_hard,hard_easy)/2)

expsupp_priodif_sav_rav <- expsupp_priodif_sin_rav %>% group_by() %>%
  summarise(sd_difacc = sd(difacc, na.rm = TRUE),
            difacc = mean(difacc, na.rm = TRUE),
            n_difacc = n()) %>%
  mutate(se_difacc = sd_difacc / sqrt(n_difacc),
         low_ci_difacc = difacc - qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc,
         up_ci_difacc = difacc + qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc)


expsupp_prio_sin_rav_spread <- expsupp_prio_sin_rav %>% select(sub,catperf,prob) %>% spread(catperf,prob)
t.test(expsupp_prio_sin_rav_spread$`90-60`,expsupp_prio_sin_rav_spread$`60-90`, paired=TRUE)
