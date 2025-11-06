rm(list = ls())
### load packages ###
{
  library(tidyverse)
  library(magrittr)
  library(psych)
  library(lme4)
  library(lmerTest)
  library(readxl)
  library(DataCombine)
  library(rmcorr)
  library(lmerTest)
  library(ICC)
  library(sjstats)
  library(sjPlot)
  library(apaTables)
  library(viridis)
}

{
#### load data ####

load("data.RData")
  
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=TRUE)
  detach(data)
  
}



{
  
  #### outlier screening & exclusion ####
  
  ## exclude participants who did not answer questions
  
  inspect_participants <- data %>%
    filter(!is.na(day)) %>%
    group_by(record_id) %>%
    summarise(mean_unique_qs = mean(n_unique_qs_shifted, na.rm = T),
              sum_unique_qs = sum(n_unique_qs_shifted, na.rm=T),
              max_day = max(day, na.rm=T)
    )
  length(which(inspect_participants$mean_unique_qs < 2 | inspect_participants$mean_unique_qs==NA))
  #7
  
  data %<>%
    filter(!record_id %in% (inspect_participants %>% filter(mean_unique_qs < 2 | is.nan(mean_unique_qs)) %>% pull(record_id)))
  
  ## exclude participants who did not report exam grades or who did not participate in mock exams
 
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=TRUE)
  detach(data)
  aggdata <-dplyr::select(aggdata, -record_id)
  aggdata <- plyr::rename(aggdata, c("Group.1" ="record_id"))
  
  #filter 1 person who indicates exam perf of 0 and 14 persons without mock exam
  length(which(aggdata$performance_exam ==0)) #1
  length(which(aggdata$performance_exam =="NaN")) #17
  data<-filter(data, performance_exam>0 & performance_exam!="NaN")
  
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=TRUE)
  detach(data)
  aggdata <-dplyr::select(aggdata, -record_id)
  aggdata <- plyr::rename(aggdata, c("Group.1" ="record_id"))
  
  length(which(aggdata$performance_mock_exams =="NaN")) #14
  data<-filter(data, performance_mock_exams!="NaN")
  
  #outliers: did not use amboss for exam preparation; 
  #169 (only 2044 qs in 100 days phase), 
  #487 answered only around 500 qs overall
  #489 answered only around 600 qs overall 
  data<-filter(data, record_id!=169 & record_id!=487 &record_id!=489 )
  

  # #suspiciously high n of qs and large drop mock -> exam; probably shared account
  
  data<-filter(data, record_id!=207 & record_id!=498 & record_id!=365 & record_id!=363 
                  & record_id!=273 & record_id!=150)
  
  rm(inspect_participants)
}  
  {
    
   
    #### data exclusion daily level #### 
   
    outlier_goalsetting <- ggplot(data=data, aes(x = "", y = goal_qs)) + geom_boxplot() +
      stat_boxplot(geom = "errorbar",
                   width = 0.4) +
      stat_summary(fun=mean, geom="point", shape=5, size=4) +
      theme(text = element_text(size=15),
            axis.text.x = element_text(hjust=1)) +
      ylab("Goal setting (no of questions)")+
      xlab("")
    plot(outlier_goalsetting)
    
    #filter observations with unrealistically high goals (>= 600) and observations where student did not set a goal (goal_qs = 0)
  
    length(which(data$goal_qs==0)) #k =47 
    length(which(data$goal_qs>600)) #3
    
    # time between pre-learning (bs) questionnaire and post-learning (as) questionnaire

    data$after_survey_time_stamp<- as.POSIXct(strptime(data$after_survey_time_stamp, format = "%d.%m.%Y %H:%M"), tz = "Europe/London")
    data$goal_setting_time_stamp<- as.POSIXct(strptime(data$goal_setting_time_stamp, format = "%d.%m.%Y %H:%M"), tz = "Europe/London")
    
  
      data %<>%
      mutate(tdiff_bs_as = as.numeric(difftime(after_survey_time_stamp, goal_setting_time_stamp, units=("hours"),
                                             )))
    plot_tdiff_bs_as <- ggplot(data) + geom_histogram(aes(x = tdiff_bs_as), 
                                                      fill = "gray", color = "black", bins = 100) +
      scale_x_continuous(breaks = seq(0, 35, 4)) +
      scale_y_continuous(breaks = seq(0, 350, 50)) +
      geom_vline(xintercept = 24, size = 2.5) +
      xlab("time between pre-learning (bs) questionnaire and post-learning (as)") +
      theme_classic() 
    plot(plot_tdiff_bs_as)
    
    
    data_obs <- filter(data, day!= "days_participated_arm_4" & day!= "invitation_arm_1" & day!= "invitation_arm_2" & day!= "invitation_arm_3" & day!= "trait_measure_1_arm_1" & day!= "trait_measure_1_arm_2" & day!= "trait_measure_1_arm_3" 
                       & day!= "trait_measure_2_arm_1" & day!= "trait_measure_2_arm_2" & day!= "trait_measure_2_arm_3")
    sum(is.na(data_obs$tdiff_bs_as))
    
    length(which(data$tdiff_bs_as>=24)) #k=77 : larger gap 
    sum(is.na(data_obs$tdiff_bs_as)) #k = 495: either before or after learning questionnaire is missing
    
    # View(data %>% select(record_id, day,after_survey_time_stamp, goal_setting_time_stamp,tdiff_bs_as
    # ))
     
    data<-data[ order( data$record_id, data$day ),  ]
    
    data<- data %<>%
      slide(Var="goal_setting_time_stamp", GroupVar="record_id", NewVar="lead.goal_setting_time_stamp", slideBy = +1, keepInvalid=TRUE, reminder=F)
    # time between post-learning (as) questionnaire and pre-learning (bs) questionnaire
    data %<>%
      mutate(tdiff_as_bs = as.numeric(difftime(lead.goal_setting_time_stamp,after_survey_time_stamp), units=("hours")))
    describe(data$tdiff_as_bs)
    length(which(data$tdiff_as_bs>24)) #k = 807 (of 9355); 9% of data

    rm(outlier_goalsetting,plot_tdiff_bs_as,data_obs)
    
  }
  
  {
    # compute gap between evening questionnaire and morning questionnaire
    data<-data[ order( data$record_id, data$day ),  ]
    
     data<- data %<>%
      slide(Var="goal_setting_time_stamp", GroupVar="record_id", NewVar="lead.goal_setting_time_stamp", slideBy = +1, keepInvalid=TRUE, reminder=F)
    # time between post-learning (as) questionnaire and pre-learning (bs) questionnaire
    data %<>%
      mutate(tdiff_as_bs = as.numeric(difftime(lead.goal_setting_time_stamp,after_survey_time_stamp), units=("hours")))
    describe(data$tdiff_as_bs)
    length(which(data$tdiff_as_bs>24)) #k = 807 (of 8771); 21% of data
    length(which(data$tdiff_as_bs<1)) #k = 141 (of 8771) ; 1% of data
    
    #### these filters were used for daily analyses ####
    #filter observations with unrealistically high goals (>= 600) and observations where student did not set a goal (goal_qs = 0)
    #filter(goal_qs < 600)
    #filter(goal_qs > 0)
    
    #filter observations where time span between "before" and "after" questionnaire exceeded 24 hours or was below 1 hour 
    #this was done to make sure that before and after questionnaire refer to the same day
    #filter(tdiff_bs_as < 25)
    #filter(tdiff_bs_as > 0)
    
    #for analysis where anxiety is predicted by prior performance:
    #include only consecutive days, i.e., filter observations where time span between "after" and next "before" questionnaire exceeded 24 hours or was below 1 hour 
    #filter(tdiff_as_bs < 25)
    #filter(tdiff_as_bs >0)
    
  }
 
  {
 #### generate scales // compute variables // centering #### 

    #test anxiety
    
    data<- data %>% 
      mutate(test_anxiety=rowMeans(.[ , c("anx_t1","anx_t2","anx_t3","anx_t4", "anx_t5")] , na.rm=TRUE))
   
    # % correctly sloved
   
    data %<>% 
      mutate(perc_correct = 1-((n_unique_qs_shifted-n_unique_qs_correct_shifted)/n_unique_qs_shifted))         
  
    
    #create lead variable for anxiety before learning
    data<- data %<>%
      slide(Var="anxiety_b", GroupVar="record_id", NewVar="lead.anxiety_b", slideBy = +1, keepInvalid=TRUE, reminder=F)
    
    # centering
    data %<>%
      group_by(record_id) %>%
      mutate(
        # person mean
        pm_working_memory = mean(working_memory_state, na.rm=T),
      ) %>%
      ungroup() %>%
      mutate(
        # grand mean
        gm_working_memory = mean(working_memory_state, na.rm=T),
        
        # grand mean centering
        working_memory_grand_mean_c = pm_working_memory - gm_working_memory)
    
     #omega 
    
    attach(data)
    aggdata<-aggregate(data, by =list(record_id),
                       FUN=mean, na.rm=TRUE)
    detach(data)
    
    omega_anxiety_t1 <- aggdata[c("anx_t1","anx_t2","anx_t3","anx_t4", "anx_t5")]
    omega(omega_anxiety_t1)
    
  
    rm(omega_anxiety_t1)
    
    #reliability state anxiety 
    
    is.even <- function(x) x %% 2 == 0
    data<- mutate(data, days2=ifelse(is.even(day), 2,1))
    
    split_half_rel_anx <- aggregate(data[,"anxiety_b"], by=list(data$record_id, data$days2), FUN=mean, na.rm=TRUE)
    names(split_half_rel_anx) <- c("record_id", "days", "anxiety_b")
    split_half_rel_anx <- split_half_rel_anx %>%
      group_by(record_id) %>%
      filter(n()>1)
    cor.test(subset(split_half_rel_anx, days==1)$anxiety_b, subset(split_half_rel_anx, days==2)$anxiety_b)
    # .97
    rm(split_half_rel_anx)

  }

{
 
  #### descriptives #####
  
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=T)
  detach(data)
  aggdata <-dplyr::select(aggdata, -record_id)
  aggdata <- plyr::rename(aggdata, c("Group.1" ="record_id"))
  
  describe(aggdata$age)  #25.90 SD = 3.25
  tabulate(aggdata$gender) #71% female

}
  {
  #### comparison between control group and experimental group ####
  # data was collected in a lrger project that included an experimental manipulation
  # therefore we tested whether control group and experimental group differed regarding the main variables of interest
  # groups did not differ significantly
  
  cg_vs_eg <- t.test(test_anxiety~ cg, data = aggdata, var.equal=T)
  cg_vs_eg
  cg_vs_eg <- t.test(performance_exam~ cg, data = aggdata, var.equal=T)
  cg_vs_eg
  cg_vs_eg <- t.test(performance_mock_exams~ cg, data = aggdata, var.equal=T)
  cg_vs_eg
  cg_vs_eg <- t.test(performance_exam_prepare~ cg, data = aggdata, var.equal=T)
  cg_vs_eg

  rm(cg_vs_eg)
  }
  
  ########## Main Results ###########
  
  {
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=T)
  detach(data)
  aggdata <-dplyr::select(aggdata, -record_id)
  aggdata <- plyr::rename(aggdata, c("Group.1" ="record_id"))
  
   #### Test Anxiety Does Not Predict Performance Drop from Mock Exams to Final Exam ####
 
   #Latent Change model was conducted in mplus (see respective script in mplus)
   # see script_lcm_perf_change_estimate_step1 &
   # script_lcm_perf_change_on_anx_step2
  {
    library(MplusAutomation)
    datalcm<-aggdata
    datalcm%<>% dplyr::select(record_id, test_anxiety,performance_mock_exams,performance_exam,
                                 anx_t1,anx_t2,anx_t3,anx_t4,anx_t5)
    write.csv(datalcm, paste0(here::here("Mplus"), "/lcmdata.csv"), row.names = F)
    datalcm <- read.csv(paste0(here::here("Mplus"), "/lcmdata.csv"))
    MplusAutomation::prepareMplusData(datalcm, filename = paste0(here::here("Mplus"), "/lcmdata.dat"),
                                      inpfile = paste0(here::here("Mplus"), "/script_lcm.inp"))
    unlink(paste0(here::here("Mplus"), "/lcmdata.csv"))
    rm(datalcm)
  }
  }

  {
  #### Test Anxiety is Negatively Related to Knowledge and Exam Performance ####
  #Table 1:
  aggdata %>% select(test_anxiety, anxiety_b, performance_exam_prepare,performance_mock_exams,performance_exam) %>% apa.cor.table(filename="Corr_table_c.doc")
  }
  {
  #### Test Anxiety Predicts Exam Performance When Entered As Sole Predictor ####
  #Table 2, Model 1: 
  perf_on_TA <- lm(performance_exam~ test_anxiety, data = aggdata)
  summary(perf_on_TA)
  effectsize::standardize_parameters(perf_on_TA)
  tab_model(perf_on_TA)
  # 4% of variance explained by TA
  
  #### Test Anxiety Does Not Predict Exam Performance Beyond Mock Exam Performance ####
  #Table 2, Model 2:
  perf_on_TA_mocks <- lm(performance_exam~ test_anxiety+performance_mock_exams, data = aggdata)
  summary(perf_on_TA_mocks)
  effectsize::standardize_parameters(perf_on_TA_mocks)
  tab_model(perf_on_TA_mocks)

  
  #### Increase in Anxiety over Time #####
  anx_over_time<- lmer(formula = anxiety_b ~ day+(1|record_id),
                                data=data %>% filter(goal_qs > 0 & goal_qs <600 & tdiff_bs_as < 25 & tdiff_bs_as >0) )
  summary(anx_over_time)
  effectsize::standardize_parameters(anx_over_time)
  tab_model(anx_over_time)
  
  #### Test Anxiety Does Not Predict Exam Performance Beyond Exam Preparation Performance ####
  #Table 2, Model 3:
  perf_on_TA_WM_trait <- lm(performance_exam~ test_anxiety*working_memory_trait+performance_mock_exams, data = aggdata)
  summary(perf_on_TA_WM_trait)
  effectsize::standardize_parameters(perf_on_TA_WM_trait)
  tab_model(perf_on_TA_WM_trait)

  #### Working Memory Capacity Does Not Moderate the Link Between Test Anxiety and Exam Performance ####
  #Table 2, Model 4:
  perf_on_TA_prep <- lm(performance_exam~ test_anxiety+performance_exam_prepare, data = aggdata)
  summary(perf_on_TA_prep)
  effectsize::standardize_parameters(perf_on_TA_prep)
  tab_model(perf_on_TA_prep)
  #see supplementary analyses at the end of script for equivalent analysis with daily working memory measure
  
  rm(perf_on_TA, perf_on_TA_mocks, perf_on_TA_prep,anx_over_time, perf_on_TA_WM_trait)
}

#### The Following Analyses Are Conducted in MPLUS ####
#### these filters were used for daily analyses ####
#filter observations with unrealistically high goals (>= 600) and observations where student did not set a goal (goal_qs = 0)
#filter(goal_qs < 600)
#filter(goal_qs > 0)

#filter observations where time span between "before" and "after" questionnaire exceeded 24 hours or was below 1 hour 
#this was done to make sure that before and after questionnaire refer to the same day
#filter(tdiff_bs_as < 25)
#filter(tdiff_bs_as > 0)

#for analysis where anxiety is predicted by prior performance:
#include only consecutive days, i.e., filter observations where time span between "after" and next "before" questionnaire exceeded 24 hours or was below 1 hour 
#filter(tdiff_as_bs < 25)
#filter(tdiff_as_bs >0)

#### export to mplus ####
  
{
#the following dataset is used to test test anxiety as moderator of increase in knowledge
# load dataset that includes daily correctly solved questions over 100 days
# was_answer_correct indicates the % of correctly solved questions on each day
load("know.RData")

#for later plotting
know %<>%
  mutate(gmAnxiety = mean(test_anxiety, na.rm=T),
         sdAnxiety = sd(test_anxiety, na.rm=T),
         modAnx = ifelse(test_anxiety < gmAnxiety- (1.5*sdAnxiety), 1, ifelse(test_anxiety > gmAnxiety+ (1.5*sdAnxiety), -1,0)))
know %<>%
  mutate(hi_anx = ifelse(modAnx==-1L, 1,0),
         lo_anx = ifelse(modAnx==1L, 1,0))

{
  library(MplusAutomation)
  know %<>% dplyr::select(record_id, days_until_exam, days_until_exam_rec, was_answer_correct,
                          anx_t1,anx_t2,anx_t3,anx_t4,anx_t5, test_anxiety, hi_anx, lo_anx)
  write.csv(know, paste0(here::here("Mplus"), "/know.csv"), row.names = F)
  know <- read.csv(paste0(here::here("Mplus"), "/know.csv"))
  MplusAutomation::prepareMplusData(know, filename = paste0(here::here("Mplus"), "/know.dat"),
                                    inpfile = paste0(here::here("Mplus"), "/script_know_over_time.inp"))
  unlink(paste0(here::here("Mplus"), "/know.csv"))
}
}
  
  
  #the following dataset is generated to conduct reciprocal links analysis
  {
    library(MplusAutomation)
    daydata<-data
    daydata %<>% dplyr::select(record_id, day, goal_qs, tdiff_bs_as, tdiff_as_bs,  anx_t1,anx_t2,anx_t3,anx_t4,anx_t5, test_anxiety,
                               anxiety_b, lead.anxiety_b,perc_correct)
    write.csv(daydata, paste0(here::here("Mplus"), "/daydata.csv"), row.names = F)
    daydata <- read.csv(paste0(here::here("Mplus"), "/daydata.csv"))
    MplusAutomation::prepareMplusData(daydata, filename = paste0(here::here("Mplus"), "/daydata.dat"),
                                      inpfile = paste0(here::here("Mplus"), "/input_daydata.inp"))
    unlink(paste0(here::here("Mplus"), "/daydata.csv"))
  }
  
#### plots ####

  #### Figure 1: development mock exam performance to exam ####
  {
    load("mock_to_exam.RData")
    
    ggplot (mock_to_exam,
            aes (x = time, y = value, line = as.factor(record_id), colour=slope)) +
      geom_line(size = .15) +
      stat_smooth( method ="loess",size = .15)+
      theme(legend.position="none")+
      scale_color_gradient2(low = "#0b5394", high = "#cc0000", mid = "#c27ba0") +
      ylab("Percentage Correctly Solved Questions") +
      scale_y_continuous(limits = c(0, 100))+
      theme(text = element_text(size=18,family="Times New Roman"),
            panel.background = element_rect(fill = 'white', colour = 'black'),
            axis.text.x = element_text(hjust=1))
  }
  
{
  #### Figure 2A: Knowledge Increase Depending on Trait Anxiety ####
  #from mplus: general intercept:  71.932
  #intercept for low anxious:  71.932 + 5.074 = 77.006
  #intercept for high anxious:  71.932 -2.262  = 69.67
  #from mplus: general slope:  0.04
  #slope for high anxious:  0.04 -0.011 = 0.029
  #slope for low anxious:  0.04 + 0.017 = 0.057

  know<-mutate(know, points = was_answer_correct*100)  
  ggplot(know,  aes(x = days_until_exam_rec, y = points, line = factor(record_id), color = test_anxiety)) + 
    geom_smooth(aes(color = test_anxiety),size = .15, method=lm, se=F) +
    scale_color_viridis(option = "H")+
    geom_abline(intercept = 77.006, slope = 0.057, size = 1,colour="blue")+
    geom_abline(intercept = 69.67, slope = 0.029, size = 1,colour="red")+
    # theme(legend.position="none")+
    xlab("Days") +
    ylab("% Correctly Solved") +
    scale_x_continuous(limits = c(0, 71),breaks=c(0,10,20, 30,40,50,60,70))+
    scale_y_continuous(limits = c(0, 100),breaks=c(0,25, 50,75, 100))+
    theme(text = element_text(size=20,family="Times New Roman"),
          panel.background = element_rect(fill = 'white', colour = 'black'),
          axis.text.x = element_text(hjust=1))
}
  ####  Figure 2B: Reciprocal Links Plots ####
  {
    data<-mutate(data, point=perc_correct*100)
    ggplot(data,  aes(x = point, y = lead.anxiety_b))+ 
      geom_abline(intercept = 2.8, slope = -0.01, size = 2,colour="tomato")+
      xlab("% Correctly Solved (T)") +
      ylab("Anxiety (T+1)") +
      scale_x_continuous(limits = c(0, 100))+
      scale_y_continuous(limits = c(1, 5))+
      theme(text = element_text(size=18,family="Arial"),
            panel.background = element_rect(fill = 'white', colour = 'black'),
            axis.text.x = element_text(hjust=1))
    
    ggplot(data,  aes(x = anxiety_b, y = point))+ 
      geom_abline(intercept = 79, slope = 0.10, size = 2,colour="tomato")+
      xlab("Anxiety (T)") +
      ylab("% Correctly Solved (T)") +
      scale_x_continuous(limits = c(1,5))+
      scale_y_continuous(limits = c(0, 100))+
      theme(text = element_text(size=18,family="Arial"),
            panel.background = element_rect(fill = 'white', colour = 'black'),
            axis.text.x = element_text(hjust=1))
  }

#### supplementary materials #####
{
#Model for Table S1 in supplementary information
perf_on_TA_WM_state <- lm(performance_exam~ test_anxiety*working_memory_grand_mean_c+performance_mock_exams, data = aggdata)
summary(perf_on_TA_WM_state)
effectsize::standardize_parameters(perf_on_TA_WM_state, method="posthoc")
tab_model(perf_on_TA_WM_state)
rm(perf_on_TA_WM_state)

#### distributions of trait test anxiety, state anxiety and working memory capacity ####

plot(ggplot(aggdata) + geom_histogram(aes(x = test_anxiety), 
                                              fill = "gray", color = "black") +
       xlab("Test Anxiety") +
       theme_classic())
plot(ggplot(aggdata) + geom_histogram(aes(x = anxiety_b), 
                                              fill = "gray", color = "black") +
       xlab("Anxiety before learning") +
       theme_classic())
plot(ggplot(aggdata) + geom_histogram(aes(x = working_memory_trait), 
                                              fill = "gray", color = "black") +
       xlab("Working memory") +
       theme_classic())

}

########## supplementary LCM with exam prep ###########

{
  attach(data)
  aggdata<-aggregate(data, by =list(record_id),
                     FUN=mean, na.rm=T)
  detach(data)
  aggdata <-dplyr::select(aggdata, -record_id)
  aggdata <- plyr::rename(aggdata, c("Group.1" ="record_id"))
  
  #Latent Change model was conducted in mplus (see respective script in mplus)
  # see script_lcm_prep_perf_change_estimate_step1 &
  # script_lcm_prep_perf_change_on_anx_step2
  {
    library(MplusAutomation)
    datalcm_prep<-aggdata
    datalcm_prep%<>% dplyr::select(record_id, test_anxiety,performance_mock_exams,performance_exam,performance_exam_prepare,
                              anx_t1,anx_t2,anx_t3,anx_t4,anx_t5)
    write.csv(datalcm_prep, paste0(here::here("Mplus"), "/datalcm_prep.csv"), row.names = F)
    datalcm_prep <- read.csv(paste0(here::here("Mplus"), "/datalcm_prep.csv"))
    MplusAutomation::prepareMplusData(datalcm_prep, filename = paste0(here::here("Mplus"), "/datalcm_prep.dat"),
                                      inpfile = paste0(here::here("Mplus"), "/script_lcm_prep.inp"))
    unlink(paste0(here::here("Mplus"), "/datalcm_prep.csv"))
    rm(datalcm_prep)
  }
}