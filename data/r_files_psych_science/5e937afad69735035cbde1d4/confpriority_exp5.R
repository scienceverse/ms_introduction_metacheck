##############CONFIDENCE AS A PRIORITY SIGNAL - DATA MANIPULATIONS FOR EXPERIMENT 5 (MENTAL ARITHMETIC)###################

#####PRIORITY####
exp5_prio_sin <- exp5_raw %>% group_by(subject) %>% mutate(easyquestfirst2 = ifelse(easyquestfirst=="1",1,0)) %>% do(calculate_proportions(.,easyquestfirst2))

exp5_prio_sav <- exp5_prio_sin %>% ungroup() %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

####OBSERVED PERFORMANCE ACCORDING TO DIFFICULTY####
exp5_perf_sin <- exp5_raw %>% select(subject,coreasy,corhard) %>% gather(key = difficulty, value = correct, -subject)
exp5_perf_sin <- exp5_perf_sin %>% group_by(subject,difficulty) %>% do(calculate_proportions(.,correct))


exp5_perf_sav <- exp5_perf_sin %>% group_by(difficulty) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

####RT ACCORDING TO DIFFICULTY####
exp5_rt_sin <- exp5_raw %>% select(subject,rteasy,rthard) %>% gather(key = difficulty, value = rt, -subject)
exp5_rt_sin <- exp5_rt_sin %>% group_by(subject,difficulty) %>% summarise(rt=mean(rt))


exp5_rt_sav <- exp5_rt_sin %>% group_by(difficulty) %>%
  summarise(sd_rt = sd(rt, na.rm = TRUE),
            rt = mean(rt, na.rm = TRUE),
            n_rt = n())

####LOCALIZATION BIAS####
exp5_raw <- exp5_raw %>% mutate(locboxrespreord = ifelse(locboxresp==0,1,0)) #so choice of left box is 1 and we can calculate proportions
exp5_loc_sin <- exp5_raw %>% group_by(subject) %>% do(calculate_proportions(.,locboxrespreord))
exp5_loc_sav <- exp5_loc_sin %>% ungroup() %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

####PERFORMANCE AS A FUNCTION OF DIFFICULTY CHOSEN FIRST####
exp5_difrte_sin <- exp5_raw %>% select(subject,easyquestfirst,rteasy) %>% mutate(dif="easy")
names(exp5_difrte_sin) <- c("subject", "easyquestfirst","rt","dif")
exp5_difrth_sin <- exp5_raw %>% select(subject,easyquestfirst,rthard) %>% mutate(dif="hard")
names(exp5_difrth_sin) <- c("subject", "easyquestfirst","rt","dif")
exp5_difrt_sin <- rbind(exp5_difrte_sin,exp5_difrth_sin)
exp5_difrt_sin <- exp5_difrt_sin %>% group_by(subject,dif,easyquestfirst) %>% summarise(rt=median(rt))


exp5_difrt_sav <- exp5_difrt_sin %>% group_by(dif,easyquestfirst) %>%
  summarise(sd_rt = sd(rt, na.rm = TRUE),
            rt = mean(rt, na.rm = TRUE),
            n_rt = n()) %>%
  mutate(se_rt = sd_rt / sqrt(n_rt),
         low_ci_rt = rt - qt(1 - (0.05 / 2), n_rt - 1) * se_rt,
         up_ci_rt = rt + qt(1 - (0.05 / 2), n_rt - 1) * se_rt)

exp5_difrt_sav <- exp5_difrt_sav %>% mutate(dif2 = ifelse(dif=="easy",1,0))
exp5_difrt_sin <- exp5_difrt_sin %>% mutate(dif2 = ifelse(dif=="easy",1,0))