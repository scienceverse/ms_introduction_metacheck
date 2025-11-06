library(tidyverse)

course_ind_df <- read_csv('../preprocessed_data/analysis.csv') %>% 
  mutate(grade = as.integer(grade)) %>% 
  mutate(user_idx = factor(user_idx),
         smoker = as.integer(smoke_freq!=4), #Changed from the original submit
         skipping = 100 - attendance,
         parent_inc_mean = parent_inc_mean / 10000,
         screentime_std=screentime/sd(screentime),
         grade_std=grade/sd(grade),
         course_num=str_remove(course_num_sem,'_.*')) %>% 
  select(user_idx,course_num_sem,course_num,grade,grade_std,screentime,screentime_std,screentime_sms,
         screentime_outofclass,screentime_nopause_v1,screentime_nopause_v2,skipping,
         male,age,hs_gpa,parent_inc_mean,parent_edu_max,conscientiousness,agreeableness,
         neuroticism,openness,extraversion,locus_of_control,bmi,smoker,study,semester)

length(unique(course_ind_df$user_idx))

if (filter_missing_background) {
  course_ind_df <- course_ind_df %>% filter(!is.na(hs_gpa), !is.na(parent_edu_max), !is.na(parent_inc_mean))
}

length(unique(course_ind_df$user_idx))

sem_course_count <- course_ind_df %>% group_by(user_idx,semester) %>% count() %>% rename(n_courses_sem=n)
if (filter_multiple_semesters) {
  course_ind_df <- course_ind_df %>% left_join(sem_course_count) %>% filter(n_courses_sem>1)
}


filter_sparse_groups <- function(df, threshold) {
  continue <- T
  while (continue) {
    before <- nrow(df)
    users_pr_course <- df %>%
      group_by(course_num_sem) %>%
      summarise(n_u = n())
    df %<>%
      left_join(users_pr_course, by = "course_num_sem") %>%
      filter(n_u>=threshold) %>%
      select(-n_u)
    courses_pr_user <- df %>%
      group_by(user_idx) %>%
      summarise(n_c = n())
    df %<>%
      left_join(courses_pr_user, by = 'user_idx') %>%
      filter(n_c>=threshold) %>%
      select(-n_c)
    after <- nrow(df)
    continue <- after < before
  }
  rm(courses_pr_user, users_pr_course, after, before, continue)
  df
}

#length(unique(course_ind_df$user_idx))
course_ind_df <- course_ind_df %>% filter_sparse_groups(2)
#length(unique(course_ind_df$user_idx))

ind_avr_df <- course_ind_df %>%
  group_by(user_idx) %>%
  summarise_if(is.numeric, mean) %>%
  rename(gpa = grade) %>%
  ungroup() %>%
  left_join(course_ind_df %>% select(user_idx, study) %>% distinct(),
            by='user_idx') %>%
  mutate(male = factor(male),
         smoker = factor(smoker),
         screentime_std=screentime/sd(screentime),
         gpa_std=gpa/sd(gpa))

course_ind_df <- course_ind_df %>% mutate(male=factor(male),smoker=factor(smoker))

socioeco <- c('parent_inc_mean', 
              'parent_edu_max')
demographic <- c('age',
                 'male')
past_perf <- c('hs_gpa')
psychological <- c("agreeableness", 
                   "extraversion", 
                   "neuroticism", 
                   "openness",
                   "conscientiousness",
                   "locus_of_control")
health <- c('bmi',
            'smoker')
ctrls <- c(past_perf,
           demographic,
           socioeco,
           health,
           psychological)
form <- paste0('~',main_pred,'+',paste0(ctrls,collapse='+'))

name_scheme <- function(data, type=c('contcat','cont','cat')) {
  if (type=='contcat') {
    return (rename(data, 'High school grade average' = hs_gpa, 'Parents\' max years of education' = parent_edu_max,
                   'Parents\' mean income' = parent_inc_mean, 'Age' = age, 'Agreeableness' = agreeableness,
                   'Conscientiousness' = conscientiousness, 'Extraversion' = extraversion, 'Neuroticism' = neuroticism,
                   'Openness' = openness, 'Locus of control' = locus_of_control, 'BMI' = bmi, 'Male' = male, 'Smoker' = smoker))
  }
  if (type=='cont') {
    return (rename(data, 'High school grade average' = hs_gpa, 'Parents\' max years of education' = parent_edu_max,
                   'Parents\' mean income' = parent_inc_mean, 'Age' = age, 'Agreeableness' = agreeableness,
                   'Conscientiousness' = conscientiousness, 'Extraversion' = extraversion, 'Neuroticism' = neuroticism,
                   'Openness' = openness, 'Locus of control' = locus_of_control, 'BMI' = bmi))
  }
  if (type=='cat') {
    return (rename(data, 'Male' = male, 'Smoker' = smoker))
  }
}

rm(sem_course_count,filter_sparse_groups,filter_missing_background)