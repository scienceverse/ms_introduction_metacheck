########################################## STATISTICAL ANALYSES FOR REPLY TO ELMER (2020) COMMENTARY #################################################

library(tidyverse)
library(lme4)

######### 58 SEC DATA ############

load("data_complete_lagged.ro")

data_all_new <- data_complete_lagged %>% group_by(user_id, date) %>%
                                         arrange(date_time) %>%
                                         mutate(mood_prev2 = ifelse(lag(obs_no_for_day)!=1, lag(mood_prev), NA)) %>%
                                         mutate(mood_plus2 = lead(mood)) %>%
                                         ungroup() %>%
                                         filter(obs_no_for_day!=1) %>%
                                         mutate(day_mean_mood_excl_current = (sum_daily_mood - mood_prev)/(day_n_obs-1)) %>%
                                         mutate(day_mean_mood_excl_subsq = (sum_daily_mood - mood_prev - mood)/(day_n_obs-2)) %>%
                                         mutate(day_mean_mood_excl_other = ifelse(!is.na(mood_prev2), 
                                                                                  (sum_daily_mood - mood_prev - mood_prev2)/(day_n_obs-2),
                                                                                  ifelse(!is.na(mood_plus2), 
                                                                                         (sum_daily_mood - mood_prev - mood_plus2)/(day_n_obs-2), 
                                                                                         NA))) %>%
                                         filter(hours_between_measures<=12 & day_n_obs>=3) # former is for consistency with original analysis

summary(data_all_new$day_avg_mood)
summary(data_all_new$day_mean_mood_excl_current)
summary(data_all_new$day_mean_mood_excl_subsq)
summary(data_all_new$day_mean_mood_excl_other)
  # check range = 0-100

# check <- data_all_new %>% filter(day_mean_mood_excl_current != day_avg_mood_no_current)
  # identical to previously calculated equivalent covariate (day_avg_mood_no_current)


##### 4+ OBS FOR DAY #####

data_58sec_tr <- data_all_new %>% filter(day_n_obs>=4)

## original model

model_58sec1 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+day_mean_mood_excl_current+people.1_prev+day_group+time_bin, 
                      data=data_58sec_tr,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec1)
exp(coef(summary(model_58sec1))['mood_prev',1])
exp(coef(summary(model_58sec1))['mood_prev',1] - 1.96*coef(summary(model_58sec1))['mood_prev',2])
exp(coef(summary(model_58sec1))['mood_prev',1] + 1.96*coef(summary(model_58sec1))['mood_prev',2])

## excluding current and subsequent happiness

model_58sec2 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+day_mean_mood_excl_subsq+people.1_prev+day_group+time_bin, 
                      data=data_58sec_tr,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec2)
exp(coef(summary(model_58sec2))['mood_prev',1])
exp(coef(summary(model_58sec2))['mood_prev',1] - 1.96*coef(summary(model_58sec2))['mood_prev',2])
exp(coef(summary(model_58sec2))['mood_prev',1] + 1.96*coef(summary(model_58sec2))['mood_prev',2])

## excluding current plus either t-1 or t+2 happiness

model_58sec3 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+day_mean_mood_excl_other+people.1_prev+day_group+time_bin, 
                      data=data_58sec_tr,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec3)
exp(coef(summary(model_58sec3))['mood_prev',1])
exp(coef(summary(model_58sec3))['mood_prev',1] - 1.96*coef(summary(model_58sec3))['mood_prev',2])
exp(coef(summary(model_58sec3))['mood_prev',1] + 1.96*coef(summary(model_58sec3))['mood_prev',2])

## happiness at time t+1 as covariate

model_58sec4 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+mood+people.1_prev+day_group+time_bin, 
                      data=data_58sec_tr,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec4)
exp(coef(summary(model_58sec4))['mood_prev',1])
exp(coef(summary(model_58sec4))['mood_prev',1] - 1.96*coef(summary(model_58sec4))['mood_prev',2])
exp(coef(summary(model_58sec4))['mood_prev',1] + 1.96*coef(summary(model_58sec4))['mood_prev',2])

## adjusted only for time of day and day of week

model_58sec5 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+people.1_prev+day_group+time_bin, 
                       data=data_58sec_tr,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec5)
exp(coef(summary(model_58sec5))['mood_prev',1])
exp(coef(summary(model_58sec5))['mood_prev',1] - 1.96*coef(summary(model_58sec5))['mood_prev',2])
exp(coef(summary(model_58sec5))['mood_prev',1] + 1.96*coef(summary(model_58sec5))['mood_prev',2])


## STORING FULL RESULTS

store <- function(model, mean=T) {
  
  if (mean) {
    blank_table <- as.data.frame(matrix(nrow=17, ncol=2))
  } else {
    blank_table <- as.data.frame(matrix(nrow=16, ncol=2))
  }
  
  for (i in seq_along(rownames(coef(summary(model))))) {
   
    blank_table[i,1] <- paste0(format(exp(coef(summary(model))[i,1]), digits=3, nsmall=2))
    blank_table[i,2] <- paste0("(",format(exp(coef(summary(model))[i,1] - 1.96*coef(summary(model))[i,2]), digits=3, nsmall=2),", ",
                                   format(exp(coef(summary(model))[i,1] + 1.96*coef(summary(model))[i,2]), digits=3, nsmall=2),")")
     
  }
  
  rownames(blank_table) <- rownames(coef(summary(model)))
  
  return(blank_table)
}

write.csv(store(model_58sec5, mean=F), file="no_daily_mean.csv")
write.csv(store(model_58sec1, mean=T), file="excl_Ht.csv")
write.csv(store(model_58sec2, mean=T), file="excl_Ht_Htp1.csv")
write.csv(store(model_58sec3, mean=T), file="excl_Ht_and_other.csv")
write.csv(store(model_58sec4, mean=T), file="Htp1.csv")


###### AIC & BIC comparison ######

library(runner)

data_all_AIC <- data_complete_lagged %>% group_by(user_id) %>%
                                         arrange(date_time_prev) %>%
                                         filter(!is.na(date_time_prev)) %>% # remove first ever observation for each person
                                         mutate(prev_day_avg_mood = runner(mood_prev, k="24 hours", lag=1, idx = as.POSIXct(date_time_prev), 
                                                                           f=function(x) {mean(x, na.rm=T)})) %>% 
                                                                            # rolling 24h average of h(t) lagged by 1 obs
                                         mutate(prev_day_avg_n = runner(mood_prev, k="24 hours", lag=1, idx = as.POSIXct(date_time_prev), 
                                                                        f=function(x) {length(x)})) %>%
                                         mutate(prev_week_avg_mood = runner(mood_prev, k="7 days", lag=1, idx = as.POSIXct(date_time_prev), 
                                                                            f=function(x) {mean(x, na.rm=T)})) %>%
                                         mutate(prev_week_avg_n = runner(mood_prev, k="7 days", lag=1, idx = as.POSIXct(date_time_prev), 
                                                                         f=function(x) {length(x)})) %>%
                                         ungroup() %>%
                                         filter(hours_between_measures<=12)

data_all_AIC$prev_day_avg_mood[is.nan(data_all_AIC$prev_day_avg_mood)]=NA
data_all_AIC$prev_day_avg_n[is.nan(data_all_AIC$prev_day_avg_n)]=NA
data_all_AIC$prev_week_avg_mood[is.nan(data_all_AIC$prev_week_avg_mood)]=NA
data_all_AIC$prev_week_avg_n[is.nan(data_all_AIC$prev_week_avg_n)]=NA

summary(data_all_AIC$day_avg_mood_no_current)
summary(data_all_AIC$prev_day_avg_mood)
summary(data_all_AIC$prev_week_avg_mood)


#### Limit so that averages based on >=2 observations ####

data_all_AIC_tr <- data_all_AIC %>% filter(!is.na(prev_day_avg_mood) & !is.na(prev_week_avg_mood) & 
                                           prev_day_avg_n>=2 & prev_week_avg_n>=2 & day_n_obs>=3)
  # note 2 obs in previous 24h will by definition result in 2 obs for previous week as well, just included for clarity

## original model

model_58sec_new1 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+day_avg_mood_no_current+people.1_prev+day_group+time_bin,
                          data=data_all_AIC_tr,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec_new1)
exp(coef(summary(model_58sec_new1))['mood_prev',1])
exp(coef(summary(model_58sec_new1))['mood_prev',1] - 1.96*coef(summary(model_58sec_new1))['mood_prev',2])
exp(coef(summary(model_58sec_new1))['mood_prev',1] + 1.96*coef(summary(model_58sec_new1))['mood_prev',2])

## model with 24h previous average

model_58sec_new2 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+prev_day_avg_mood+people.1_prev+day_group+time_bin,
                          data=data_all_AIC_tr,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec_new2)
exp(coef(summary(model_58sec_new2))['mood_prev',1])
exp(coef(summary(model_58sec_new2))['mood_prev',1] - 1.96*coef(summary(model_58sec_new2))['mood_prev',2])
exp(coef(summary(model_58sec_new2))['mood_prev',1] + 1.96*coef(summary(model_58sec_new2))['mood_prev',2])

## model with 7 day previous average

model_58sec_new3 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+prev_week_avg_mood+people.1_prev+day_group+time_bin,
                          data=data_all_AIC_tr,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec_new3)
exp(coef(summary(model_58sec_new3))['mood_prev',1])
exp(coef(summary(model_58sec_new3))['mood_prev',1] - 1.96*coef(summary(model_58sec_new3))['mood_prev',2])
exp(coef(summary(model_58sec_new3))['mood_prev',1] + 1.96*coef(summary(model_58sec_new3))['mood_prev',2])

## model with happiness at t+1

model_58sec_new4 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+mood+people.1_prev+day_group+time_bin,
                          data=data_all_AIC_tr,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec_new4)
exp(coef(summary(model_58sec_new4))['mood_prev',1])
exp(coef(summary(model_58sec_new4))['mood_prev',1] - 1.96*coef(summary(model_58sec_new4))['mood_prev',2])
exp(coef(summary(model_58sec_new4))['mood_prev',1] + 1.96*coef(summary(model_58sec_new4))['mood_prev',2])

## model adjusting only for day of week and time of day

model_58sec_new5 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+people.1_prev+day_group+time_bin,
                          data=data_all_AIC_tr,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec_new5)
exp(coef(summary(model_58sec_new5))['mood_prev',1])
exp(coef(summary(model_58sec_new5))['mood_prev',1] - 1.96*coef(summary(model_58sec_new5))['mood_prev',2])
exp(coef(summary(model_58sec_new5))['mood_prev',1] + 1.96*coef(summary(model_58sec_new5))['mood_prev',2])



### ORIGINAL MODEL ONLY IN INCREASERS #####

increase_58sec <- data_complete_lagged %>% filter(mood>mood_prev & hours_between_measures<=12)

increasers_model<-glmer(people.1~(1+mood_prev|user_id)+mood_prev+day_avg_mood_no_current+people.1_prev+day_group+time_bin,
                        data=increase_58sec,family=binomial('logit'), nAGQ = 0, verbose=2)

summary(increasers_model)
exp(coef(summary(increasers_model))['mood_prev',1])
exp(coef(summary(increasers_model))['mood_prev',1] - 1.96*coef(summary(increasers_model))['mood_prev',2])
exp(coef(summary(increasers_model))['mood_prev',1] + 1.96*coef(summary(increasers_model))['mood_prev',2])


###############################################################################################################################################


###### WHO SAGE DATASET #######

load("WHO.ro")
  
# some variables are already included but are based on all obs, not just those within the day
# note that because of how they were coded in the original datasets, some obs start on 5th of day etc, hence the correction below

data_full_lagged_obs2plus <- data_full_lagged %>% group_by(id) %>%
                                                  add_count(name="day_n_obs") %>%
                                                  arrange(obs_no_for_day) %>%
                                                  mutate(obs_no_for_day_new = row_number()) %>% # some e.g. start at 4 or 5
                                                  mutate(sum_moods = sum(mood)) %>%
                                                  mutate(min_mood = min(mood)) %>%
                                                  mutate(max_mood = max(mood)) %>%
                                                  mutate(mood_prev2 = lag(mood_prev)) %>%
                                                  mutate(alone_prev2 = lag(alone_prev)) %>%
                                                  mutate(act_dur_time_prev_hours = as.numeric(act_dur_time_prev/3600)) %>%
                                                  mutate(act_dur_time_prev2 = lag(act_dur_time_prev)) %>% 
                                                    # for e.g. relating happiness 1 --> activity 3, need to control for length of activity 2
                                                  mutate(act_dur_time_sum = (as.numeric(act_dur_time_prev + act_dur_time_prev2)/3600)) %>% 
                                                    # for e.g. relating happiness 1 --> activity 3, need to control for length of activity 2 and 3
                                                  mutate(mood_prev3 = lag(mood_prev2)) %>%
                                                  mutate(alone_prev3 = lag(alone_prev2)) %>%
                                                  mutate(mood_prev4 = lag(mood_prev3)) %>%
                                                  ungroup() %>%
                                                  filter(obs_no_for_day_new!=1) %>% # no previous activities/moods for first obs
                                                  mutate(mean_mood_excl_current = (sum_moods - mood_prev)/(day_n_obs-1)) %>%
                                                  mutate(mean_mood_excl_current2 = (sum_moods - mood_prev2)/(day_n_obs-1)) %>%
                                                  mutate(mean_mood_excl_current3 = (sum_moods - mood_prev3)/(day_n_obs-1)) %>%
                                                  filter(!is.na(alone_prev) & !is.na(alone) & signif(mean_mood_excl_current,3)<=2.00)
                                                    # 1 obs removed due to latter condition

summary(data_full_lagged_obs2plus$mean_mood_excl_current)
  # make sure distribution between -2 and +2
summary(data_full_lagged_obs2plus$act_dur_time_prev_hours)
summary(data_full_lagged_obs2plus$act_dur_time_sum)
                    


##### Exploring temporal delay ####


data_WHO_tr2 <- data_full_lagged_obs2plus %>% 
                filter(!is.na(mood_prev2) & !is.na(alone_prev2) & mean_mood_excl_current2<=2) %>%  # 5 obs dropped for latter condition
                group_by(id) %>%
                arrange(obs_no_for_day_new) %>%
                mutate(obs_no_for_day_new2=row_number()) %>%
                mutate(mean_mood_past = cumsum(mood_prev2)/(obs_no_for_day_new2)) %>% # average of all those moods prior to that at t-1
                ungroup()

summary(data_WHO_tr2$mean_mood_excl_current2)
summary(data_WHO_tr2$act_dur_time_prev_hours)
summary(data_WHO_tr2$mean_mood_past)

data_WHO_tr3 <- data_WHO_tr2 %>% 
                filter(!is.na(mood_prev3) & !is.na(alone_prev3) & mean_mood_excl_current3<=2) %>%
                group_by(id) %>%
                arrange(obs_no_for_day_new2) %>%
                mutate(obs_no_for_day_new2=row_number()) %>%
                mutate(mean_mood_past2 = cumsum(mood_prev3)/(obs_no_for_day_new2)) %>% # average of all those moods prior to that at t-1
                ungroup()

summary(data_WHO_tr3$mean_mood_excl_current3)
summary(data_WHO_tr3$mean_mood_past2)
summary(data_WHO_tr3$act_dur_time_sum)

model_WHO1 <- glmer(alone~(1+mood_prev|id)+mood_prev+alone_prev+time_bin, 
                    data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO1)
exp(coef(summary(model_WHO1))['mood_prev',1])
exp(coef(summary(model_WHO1))['mood_prev',1] - 1.96*coef(summary(model_WHO1))['mood_prev',2])
exp(coef(summary(model_WHO1))['mood_prev',1] + 1.96*coef(summary(model_WHO1))['mood_prev',2])

model_WHO2 <- glmer(alone~(1+mood_prev|id)+mood_prev+mood+alone_prev+time_bin, 
                     data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO2)
exp(coef(summary(model_WHO2))['mood_prev',1])
exp(coef(summary(model_WHO2))['mood_prev',1] - 1.96*coef(summary(model_WHO2))['mood_prev',2])
exp(coef(summary(model_WHO2))['mood_prev',1] + 1.96*coef(summary(model_WHO2))['mood_prev',2])

model_WHO3 <- glmer(alone~(1+mood_prev|id)+mood_prev+alone_prev+mean_mood_excl_current+time_bin, 
                     data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO3)
exp(coef(summary(model_WHO3))['mood_prev',1])
exp(coef(summary(model_WHO3))['mood_prev',1] - 1.96*coef(summary(model_WHO3))['mood_prev',2])
exp(coef(summary(model_WHO3))['mood_prev',1] + 1.96*coef(summary(model_WHO3))['mood_prev',2])


# happiness at t to alone at t+2

model_WHO4 <- glmer(alone~(1+mood_prev2|id)+mood_prev2+alone_prev2+time_bin, 
                    data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO4)
exp(coef(summary(model_WHO4))['mood_prev2',1])
exp(coef(summary(model_WHO4))['mood_prev2',1] - 1.96*coef(summary(model_WHO4))['mood_prev2',2])
exp(coef(summary(model_WHO4))['mood_prev2',1] + 1.96*coef(summary(model_WHO4))['mood_prev2',2])

model_WHO5 <- glmer(alone~(1+mood_prev2|id)+mood_prev2+mood+alone_prev2+time_bin, 
                    data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO5)
exp(coef(summary(model_WHO5))['mood_prev2',1])
exp(coef(summary(model_WHO5))['mood_prev2',1] - 1.96*coef(summary(model_WHO5))['mood_prev2',2])
exp(coef(summary(model_WHO5))['mood_prev2',1] + 1.96*coef(summary(model_WHO5))['mood_prev2',2])

model_WHO6 <- glmer(alone~(1+mood_prev2|id)+mood_prev2+alone_prev2+mean_mood_excl_current2+time_bin, 
                    data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO6)
exp(coef(summary(model_WHO6))['mood_prev2',1])
exp(coef(summary(model_WHO6))['mood_prev2',1] - 1.96*coef(summary(model_WHO6))['mood_prev2',2])
exp(coef(summary(model_WHO6))['mood_prev2',1] + 1.96*coef(summary(model_WHO6))['mood_prev2',2])


# happiness at t to alone at t+3

model_WHO7 <- glmer(alone~(1+mood_prev3|id)+mood_prev3+alone_prev3+time_bin, 
                    data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO7)
exp(coef(summary(model_WHO7))['mood_prev3',1])
exp(coef(summary(model_WHO7))['mood_prev3',1] - 1.96*coef(summary(model_WHO7))['mood_prev3',2])
exp(coef(summary(model_WHO7))['mood_prev3',1] + 1.96*coef(summary(model_WHO7))['mood_prev3',2])

model_WHO8 <- glmer(alone~(1+mood_prev3|id)+mood_prev3+mood+alone_prev3+time_bin, 
                    data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO8)
exp(coef(summary(model_WHO8))['mood_prev3',1])
exp(coef(summary(model_WHO8))['mood_prev3',1] - 1.96*coef(summary(model_WHO8))['mood_prev3',2])
exp(coef(summary(model_WHO8))['mood_prev3',1] + 1.96*coef(summary(model_WHO8))['mood_prev3',2])

model_WHO9 <- glmer(alone~(1+mood_prev3|id)+mood_prev3+mean_mood_excl_current3+alone_prev3+time_bin,
                    data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO9)
exp(coef(summary(model_WHO9))['mood_prev3',1])
exp(coef(summary(model_WHO9))['mood_prev3',1] - 1.96*coef(summary(model_WHO9))['mood_prev3',2])
exp(coef(summary(model_WHO9))['mood_prev3',1] + 1.96*coef(summary(model_WHO9))['mood_prev3',2])




#####################################################################################################################################

### SUPPLEMENTARY ANALYSES

##### 3+ OBS FOR DAY COVARIATE COMPARISON #####

data_58sec <- data_all_new

## original model (excl happiness at t)

model_58sec6 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+day_mean_mood_excl_current+people.1_prev+day_group+time_bin, 
                      data=data_58sec,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec6)
exp(coef(summary(model_58sec6))['mood_prev',1])
exp(coef(summary(model_58sec6))['mood_prev',1] - 1.96*coef(summary(model_58sec6))['mood_prev',2])
exp(coef(summary(model_58sec6))['mood_prev',1] + 1.96*coef(summary(model_58sec6))['mood_prev',2])

## excluding happiness at t and t+1

model_58sec7 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+day_mean_mood_excl_subsq+people.1_prev+day_group+time_bin, 
                      data=data_58sec,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec7)
exp(coef(summary(model_58sec7))['mood_prev',1])
exp(coef(summary(model_58sec7))['mood_prev',1] - 1.96*coef(summary(model_58sec7))['mood_prev',2])
exp(coef(summary(model_58sec7))['mood_prev',1] + 1.96*coef(summary(model_58sec7))['mood_prev',2])

## excluding current plus either t-1 or t+2 happiness

model_58sec8 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+day_mean_mood_excl_other+people.1_prev+day_group+time_bin, 
                      data=data_58sec,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec8)
exp(coef(summary(model_58sec8))['mood_prev',1])
exp(coef(summary(model_58sec8))['mood_prev',1] - 1.96*coef(summary(model_58sec8))['mood_prev',2])
exp(coef(summary(model_58sec8))['mood_prev',1] + 1.96*coef(summary(model_58sec8))['mood_prev',2])

## happiness at time t+1 as covariate

model_58sec9 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+mood+people.1_prev+day_group+time_bin, 
                      data=data_58sec,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec9)
exp(coef(summary(model_58sec9))['mood_prev',1])
exp(coef(summary(model_58sec9))['mood_prev',1] - 1.96*coef(summary(model_58sec9))['mood_prev',2])
exp(coef(summary(model_58sec9))['mood_prev',1] + 1.96*coef(summary(model_58sec9))['mood_prev',2])

## adjusted only for time of day and day of week

model_58sec10 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+people.1_prev+day_group+time_bin, 
                      data=data_58sec,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec10)
exp(coef(summary(model_58sec10))['mood_prev',1])
exp(coef(summary(model_58sec10))['mood_prev',1] - 1.96*coef(summary(model_58sec10))['mood_prev',2])
exp(coef(summary(model_58sec10))['mood_prev',1] + 1.96*coef(summary(model_58sec10))['mood_prev',2])


#### COMPLETELY UNADJUSTED MODELS #########

model_58sec_unadj1 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev,data=data_58sec_tr,
                            family=binomial('logit'), nAGQ = 0, verbose=2) # 4+ obs
summary(model_58sec_unadj1)
exp(coef(summary(model_58sec_unadj1))['mood_prev',1])
exp(coef(summary(model_58sec_unadj1))['mood_prev',1] - 1.96*coef(summary(model_58sec_unadj1))['mood_prev',2])
exp(coef(summary(model_58sec_unadj1))['mood_prev',1] + 1.96*coef(summary(model_58sec_unadj1))['mood_prev',2])

model_58sec_unadj2 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev,
                            data=data_58sec,family=binomial('logit'), nAGQ = 0, verbose=2) # 3+ obs
summary(model_58sec_unadj2)
exp(coef(summary(model_58sec_unadj2))['mood_prev',1])
exp(coef(summary(model_58sec_unadj2))['mood_prev',1] - 1.96*coef(summary(model_58sec_unadj2))['mood_prev',2])
exp(coef(summary(model_58sec_unadj2))['mood_prev',1] + 1.96*coef(summary(model_58sec_unadj2))['mood_prev',2])




### AIC AND BIC IN FULL SAMPLE ####


data_all_AIC_tr2 <- data_all_AIC %>% filter(!is.na(prev_day_avg_mood))
  # 154,520 observations

## original model

model_58sec_new6 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+day_avg_mood_no_current+people.1_prev+day_group+time_bin,
                          data=data_all_AIC_tr2,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec_new6)
exp(coef(summary(model_58sec_new6))['mood_prev',1])
exp(coef(summary(model_58sec_new6))['mood_prev',1] - 1.96*coef(summary(model_58sec_new6))['mood_prev',2])
exp(coef(summary(model_58sec_new6))['mood_prev',1] + 1.96*coef(summary(model_58sec_new6))['mood_prev',2])

## model with 24h previous average

model_58sec_new7 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+prev_day_avg_mood+people.1_prev+day_group+time_bin,
                          data=data_all_AIC_tr2,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec_new7)
exp(coef(summary(model_58sec_new7))['mood_prev',1])
exp(coef(summary(model_58sec_new7))['mood_prev',1] - 1.96*coef(summary(model_58sec_new7))['mood_prev',2])
exp(coef(summary(model_58sec_new7))['mood_prev',1] + 1.96*coef(summary(model_58sec_new7))['mood_prev',2])

## model with 7 day previous average

model_58sec_new8 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+prev_week_avg_mood+people.1_prev+day_group+time_bin,
                          data=data_all_AIC_tr2,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec_new8)
exp(coef(summary(model_58sec_new8))['mood_prev',1])
exp(coef(summary(model_58sec_new8))['mood_prev',1] - 1.96*coef(summary(model_58sec_new8))['mood_prev',2])
exp(coef(summary(model_58sec_new8))['mood_prev',1] + 1.96*coef(summary(model_58sec_new8))['mood_prev',2])

## model with happiness at t+1

model_58sec_new9 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+mood+people.1_prev+day_group+time_bin,
                          data=data_all_AIC_tr2,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec_new9)
exp(coef(summary(model_58sec_new9))['mood_prev',1])
exp(coef(summary(model_58sec_new9))['mood_prev',1] - 1.96*coef(summary(model_58sec_new9))['mood_prev',2])
exp(coef(summary(model_58sec_new9))['mood_prev',1] + 1.96*coef(summary(model_58sec_new9))['mood_prev',2])

## model adjusting only for day of week and time of day

model_58sec_new10 <- glmer(people.1~(1+mood_prev|user_id)+mood_prev+people.1_prev+day_group+time_bin,
                           data=data_all_AIC_tr2,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_58sec_new10)
exp(coef(summary(model_58sec_new10))['mood_prev',1])
exp(coef(summary(model_58sec_new10))['mood_prev',1] - 1.96*coef(summary(model_58sec_new10))['mood_prev',2])
exp(coef(summary(model_58sec_new10))['mood_prev',1] + 1.96*coef(summary(model_58sec_new10))['mood_prev',2])




################################ SUPPLEMENTARY TEMPORAL DELAY ANALYSES ###########################################

## CONTROLLING FOR DURATION OF ACTIVITIES IN BETWEEN LAGGED MEASURES (minimally adjusted models for lag 2 & 3) ##

model_WHO_duration1 <- glmer(alone~(1+mood_prev2|id)+mood_prev2+alone_prev2+time_bin+act_dur_time_prev_hours, 
                             data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO_duration1)
exp(coef(summary(model_WHO_duration1))['mood_prev2',1])
exp(coef(summary(model_WHO_duration1))['mood_prev2',1] - 1.96*coef(summary(model_WHO_duration1))['mood_prev2',2])
exp(coef(summary(model_WHO_duration1))['mood_prev2',1] + 1.96*coef(summary(model_WHO_duration1))['mood_prev2',2])


model_WHO_duration2 <- glmer(alone~(1+mood_prev3|id)+mood_prev3+alone_prev3+time_bin+act_dur_time_sum, 
                             data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO_duration2)
exp(coef(summary(model_WHO_duration2))['mood_prev3',1])
exp(coef(summary(model_WHO_duration2))['mood_prev3',1] - 1.96*coef(summary(model_WHO_duration2))['mood_prev3',2])
exp(coef(summary(model_WHO_duration2))['mood_prev3',1] + 1.96*coef(summary(model_WHO_duration2))['mood_prev3',2])


## ADJUSTING ONLY FOR AVG MOOD OCCURRING PREVIOUSLY (in lagged datasets)

model_WHO_prevavg1 <- glmer(alone~(1+mood_prev|id)+mood_prev+alone_prev+mean_mood_past+time_bin, 
                            data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO_prevavg1)
exp(coef(summary(model_WHO_prevavg1))['mood_prev',1])
exp(coef(summary(model_WHO_prevavg1))['mood_prev',1] - 1.96*coef(summary(model_WHO_prevavg1))['mood_prev',2])
exp(coef(summary(model_WHO_prevavg1))['mood_prev',1] + 1.96*coef(summary(model_WHO_prevavg1))['mood_prev',2])


model_WHO_prevavg2 <- glmer(alone~(1+mood_prev2|id)+mood_prev2+alone_prev2+mean_mood_past2+time_bin, 
                            data=data_WHO_tr3,family=binomial('logit'), nAGQ = 0, verbose=2)
summary(model_WHO_prevavg2)
exp(coef(summary(model_WHO_prevavg2))['mood_prev2',1])
exp(coef(summary(model_WHO_prevavg2))['mood_prev2',1] - 1.96*coef(summary(model_WHO_prevavg2))['mood_prev2',2])
exp(coef(summary(model_WHO_prevavg2))['mood_prev2',1] + 1.96*coef(summary(model_WHO_prevavg2))['mood_prev2',2])


## final model needs smaller subset

data_WHO_tr4 <- data_WHO_tr3 %>% 
                filter(!is.na(mood_prev4)) %>%
                group_by(id) %>%
                arrange(obs_no_for_day_new2) %>%
                mutate(obs_no_for_day_new2=row_number()) %>%
                mutate(mean_mood_past3 = cumsum(mood_prev4)/(obs_no_for_day_new2)) %>% # average of all those moods prior to that at t-1
                ungroup()

summary(data_WHO_tr4$mean_mood_past3)

count(distinct(data_WHO_tr4, id))
  # n=13,606 individuals and 23,678 observations, so more random effects than observations if we include random slopes (i.e. 2 per individual)
 
model_WHO_prevavg3 <- glmer(alone~(1|id)+mood_prev3+mean_mood_past3+alone_prev3+time_bin, 
                            data=data_WHO_tr4,family=binomial('logit'), nAGQ = 0, verbose=2) ## note different model, random slopes cannot be computed
summary(model_WHO_prevavg3)
exp(coef(summary(model_WHO_prevavg3))['mood_prev3',1])
exp(coef(summary(model_WHO_prevavg3))['mood_prev3',1] - 1.96*coef(summary(model_WHO_prevavg3))['mood_prev3',2])
exp(coef(summary(model_WHO_prevavg3))['mood_prev3',1] + 1.96*coef(summary(model_WHO_prevavg3))['mood_prev3',2])


###########################################################################################################################