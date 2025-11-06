##############STATISTICS CONFIDENCE AS A PRIORITY SIGNAL###################

#function spitting stats in the reported format. It includes a line that calculates Cohen's d for correlated samples
do_report_ttest <- function(kk){
  degfr <- format(round(kk$parameter, 0), nsmall = 0)
  tstat <- format(round(kk$statistic, 3), nsmall = 3)
  pval <- format(round(kk$p.value, 3), nsmall = 3)
  confintlow <- format(round(kk$conf.int[1], 3), nsmall = 3)
  confintup <- format(round(kk$conf.int[2], 3), nsmall = 3)
  cohensd <- kk$statistic / (sqrt(kk$parameter + 1))
  cohensd <- format(round(cohensd, 3), nsmall = 3)
  sprintf("t(%s) = %s, p = %s, 95 perc CI [%s, %s], Cohen's d = %s", degfr, tstat, pval, confintlow, confintup, cohensd)
}#t-test

####EXPERIMENT 1 - CONFIDENCE RATINGS####
#confidence ratings associated to first choice are bigger than those associated to second choice
exp1_ttest1 <- t.test(exp1_conffise_sin_rav$conffirst,exp1_conffise_sin_rav$confsecond,paired=TRUE) 
do_report_ttest(exp1_ttest1)

#t-test proportion trials where first choice had greater confidence than second choice
exp1_ttest2 <- t.test(exp1_confhigher_all_correct_sin_rav$prob, mu=0.5)#for all trial types
do_report_ttest(exp1_ttest2)
exp1_ttest3 <- t.test(exp1_confhigher_samediffcollapsed_correct_sin_rav$prob, mu=0.5)#only for EE and HH, correct
do_report_ttest(exp1_ttest3)
exp1_ttest4 <- t.test(exp1_confhigher_samediffcollapsed_wrong_sin_rav$prob, mu=0.5)#only for EE and HH, errors
do_report_ttest(exp1_ttest4)

#logistic regression (priority as a function of expected difficulty, performance and confidence) and model comparison
exp1_logistdf <- exp1_raw %>% 
  mutate(difdifq = ifelse(catperf=="90-60",30,ifelse(catperf=="60-90",-30,0)), #30 = color easy, letter hard, -30 = color hard, letter easy, 0 both tasks same difficulty
         difcorrect = ifelse(correctcol==1 & correctlet==0,1, ifelse(correctcol==0 & correctlet==1,-1,0)), #1 = color task correct & letter incorrect, -1 = color task incorrect & letter correct, 0 = both either correct or incorrect
         difconf = hconf - vconf, #positive = confidence higher for color task
         subf = as.character(paste("S",sub)),
         choice1binf = as.factor(choice1bin), #choice1binf: 1 = color chosen, 0 = letter chosen
         difcorrect100 = difcorrect*100)#multiply performance by 100 so it is in the same scale as other variables  

modfull <- glmer(choice1binf ~ difdifq + difcorrect100 + difconf + (1|subf), data = exp1_logistdf,family = binomial)
modpart <- glmer(choice1binf ~ difdifq + difcorrect100 + (1|subf), data = exp1_logistdf,family = binomial)
summary(modfull)
summary(modpart)
anova(modfull,modpart)

mod0 <- lmer(difconf ~ difdifq + difcorrect100 + (1|subf), data = exp1_logistdf)
summary(glmer(choice1binf ~  resid(mod0) + (1|subf), data = exp1_logistdf,family = binomial, offset = predict(modpart) ))

#summary(lmer(residuals(modpart) ~ residuals(mod0) + (1|as.factor(exp1_logistdf$subf))))
#summary(lm(residuals(modpart) ~ residuals(mod0)))
#summary(glmer(choice1binf ~ predict(modpart) + difconf + (1|subf), data = exp1_logistdf,family = binomial))
#summary(glmer(choice1binf ~  difconf + (1|subf), data = exp1_logistdf,family = binomial, offset = predict(modpart) ))

#same logistic regression and model comparison as before, but only with different difficulty trial types
exp1_logistdf_difdiff <- exp1_raw %>% filter(catperf=="90-60" | catperf=="60-90") %>% 
  mutate(difdifq = ifelse(catperf=="90-60",30,-30), 
         difcorrect = ifelse(correctcol==1 & correctlet==0,1, ifelse(correctcol==0 & correctlet==1,-1,0)),
         difconf = hconf - vconf,
         subf = as.character(paste("S",sub)),
         choice1binf = as.factor(choice1bin),
         difcorrect100 = difcorrect*100)
exp1_logistdf_difdiff$choice1binf <- as.factor(exp1_logistdf_difdiff$choice1bin)

modfull_difdiff <- glmer(choice1binf ~ difdifq + difcorrect100 + difconf + (1|subf), data = exp1_logistdf_difdiff,family = binomial)
modpart_difdiff <- glmer(choice1binf ~ difdifq + difcorrect100 + (1|subf), data = exp1_logistdf_difdiff,family = binomial)
summary(modfull_difdiff)
summary(modpart_difdiff)
anova(modfull_difdiff,modpart_difdiff)

mod0_difdiff <- lmer(difconf ~ difdifq + difcorrect100 + (1|subf), data = exp1_logistdf_difdiff)
summary(glmer(choice1binf ~  resid(mod0_difdiff) + (1|subf), data = exp1_logistdf_difdiff,family = binomial, offset = predict(modpart_difdiff) ))

#prioritizing high-confidence errors over low-confidence correct trials
exp1_prierror %>% ungroup() %>% summarise(ntrials = sum(ntrials), nhits = sum(nhits)) #all trials pooled together
exp1_prierror %>% filter (catperf == "60-60" | catperf == "90-90") %>% ungroup() %>% summarise(ntrials = sum(ntrials), nhits = sum(nhits)) # same difficulty trials pooled together

binom.test(nsuccesses, ntrials, 0.5, alternative="greater")#replace nsuccesses and ntrials by the output of the preceding analyses

####EXPERIMENT 2- CHOICE-ONLY####
#paired t-test proportion trials color chosen first, EH vs HE
exp2_prio_sav_rav <- exp2_prio_sav_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
exp2_prio_sin_rav <- exp2_prio_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
exp2_dfdifdif <- exp2_prio_sin_rav %>% ungroup() %>% select(sub,catperf2,prob) %>% filter(catperf2=="EH"|catperf2=="HE") %>% spread(catperf2,prob)
exp2_ttest1 <- t.test(exp2_dfdifdif$EH,exp2_dfdifdif$HE,paired=TRUE)
do_report_ttest(exp2_ttest1)

# #one-tailed t-test difference of proportions EH - HE > 0 (priority effect)
# exp2_ttest2 <- t.test(exp2_priodif_sin_rav$difacc, mu=0, alternative="greater")
# do_report_ttest(exp2_ttest2)

####EXPERIMENT DELAYS####
#t-tests 2x3 ANOVA, proportion color task chosen first as a function of trial type (EH vs HE) and delay (0,2,4s)
exp3_priodelayEHHE_sin_rav <- exp3_priodelay_sin_rav %>% filter(catperf == "60-90" | catperf == "90-60") %>% mutate(catperf2 = ifelse(catperf=="90-60","EH","HE"))
exp3_priodelayEHHE_sin_rav$delayf <- factor(exp3_priodelayEHHE_sin_rav$delay)
exp3_priodelayEHHE_sin_rav$catperf2f <- factor(exp3_priodelayEHHE_sin_rav$catperf2)
exp3_priodelayEHHE_sin_rav$sub <- as.character(paste("S",exp3_priodelayEHHE_sin_rav$sub))
summary(aov(prob ~ catperf2f * delayf + Error(sub / (catperf2f * delayf)), data = exp3_priodelayEHHE_sin_rav))

0.05629 / (0.05629+0.26771) #partial eta squared  trial type 
0.2983 / (0.2983+0.6778) #partial eta squared  delay

####EXPERIMENTS 1, 2 and 3 COMBINED ANALYSIS####
#reversal priority effect from difficulty for errors
exp123_priosameaccdif_sin_rav <- rbind(exp1_priosameaccdif_sin_rav,exp2_priosameaccdif_sin_rav,exp3_priosameaccdif_sin_rav)

exp123_priosameaccdif_sav_rav <- exp123_priosameaccdif_sin_rav %>% group_by(acccategory) %>%
  summarise(sd_difacc = sd(difacc, na.rm = TRUE),
            difacc = mean(difacc, na.rm = TRUE),
            n_difacc = n()) %>%
  mutate(se_difacc = sd_difacc / sqrt(n_difacc),
         low_ci_difacc = difacc - qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc,
         up_ci_difacc = difacc + qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc)

exp123_priobothcorrectdif_sin_rav <- exp123_priosameaccdif_sin_rav %>% filter(acccategory=="correct")
exp123_priobotherrordif_sin_rav <- exp123_priosameaccdif_sin_rav %>% filter(acccategory=="error")

exp123_ttest2 <- t.test(exp123_priobothcorrectdif_sin_rav$difacc, mu=0) #prioritization of easy tasks when both responses were correct
do_report_ttest(exp123_ttest2)

exp123_ttest3 <- t.test(exp123_priobotherrordif_sin_rav$difacc, mu=0) #prioritization of hard tasks when both responses were errors
do_report_ttest(exp123_ttest3)

exp123_priosameaccdifspread_sin_rav <- exp123_priosameaccdif_sin_rav %>% group_by(sub,acccategory) %>% select(sub,acccategory,difacc) %>% group_by(sub) %>% spread(acccategory,difacc)

exp123_ttest4 <- t.test(exp123_priosameaccdifspread_sin_rav$correct, exp123_priosameaccdifspread_sin_rav$error, paired=TRUE) #the two last measures are different
do_report_ttest(exp123_ttest4)

####EXPERIMENT 4 - PROSPECTIVE CONFIDENCE####
#one-tailed t-test proportion blocks where easy set chosen first > 0.5
exp4_chdif_collapsed_sin_rav <- exp4_tcraw %>% group_by(sub) %>% do(calculate_proportions(.,choicedif))
exp4_ttest1 <- t.test(exp4_chdif_collapsed_sin_rav$prob, mu=0.5)
do_report_ttest(exp4_ttest1)

#paired t-test between same-task and different-task blocks to show
exp4_chdif_typetaskspread_sin_rav <- ungroup(exp4_chdif_typetask_sin_rav)  %>% mutate (type =ifelse(task1_type==0,"different","same")) %>% select(sub,type,prob) %>% spread(type,prob)
exp4_ttest2 <- t.test(exp4_chdif_typetaskspread_sin_rav$same,exp4_chdif_typetaskspread_sin_rav$different,paired=TRUE) 
do_report_ttest(exp4_ttest2)

#Bayes factor between same-task and different-task blocks to show
ttestBF(x = exp4_chdif_typetaskspread_sin_rav$different, y = exp4_chdif_typetaskspread_sin_rav$same, paired=TRUE)

####EXPERIMENT 5 - MENTAL CALCULATION####
#t-test accuracy for easy problems equal than for difficult problems
exp5_perfspread_sin <- exp5_perf_sin %>% group_by(subject) %>% select (subject,difficulty,prob) %>% spread(difficulty,prob)
exp5_ttest1 <- t.test(exp5_perfspread_sin$coreasy, exp5_perfspread_sin$corhard, paired=TRUE)
do_report_ttest(exp5_ttest1)

#Bayes factor for easy problems equal than for difficult problems
ttestBF(x = exp5_perfspread_sin$coreasy, y = exp5_perfspread_sin$corhard, paired=TRUE)

#t-test RT for easy problems shorter than for difficult problems
exp5_rtspread_sin <- exp5_rt_sin %>% group_by(subject) %>% spread(difficulty,rt)
exp5_ttest2 <- t.test(exp5_rtspread_sin$rteasy, exp5_rtspread_sin$rthard,paired=TRUE)
do_report_ttest(exp5_ttest2)

#one-tailed t-test proportion trials where easy problem chosen first > 0.5
exp5_ttest3 <- t.test(exp5_prio_sin$prob, mu=0.5)
do_report_ttest(exp5_ttest3)

######################################SUPPLEMENTARY MATERIAL##################################

####1.2. Mean and SD of stimulus elements####
##for experiments 1 to 3
exp2_raw <- exp2_raw %>% mutate(exnum = 2)
exp1_raw <- exp1_raw %>% mutate(exnum = 1)
exp3_raw <- exp3_raw %>% mutate(exnum = 3)
exp123_numeldf_sin_rav <- rbind(exp2_raw,exp1_raw,exp3_raw)
exp123_numeldf_sin_rav <- exp123_numeldf_sin_rav %>% filter( catperf == "90-60" | catperf == "60-90")
exp123_numeldf_sin_rav <- exp123_numeldf_sin_rav %>% filter(number_blues > 40 & number_os > 40)
exp123_numeldf_sin_rav <- exp123_numeldf_sin_rav %>% group_by(sub,exnum) %>% summarise (col60=min(number_blues),col90=max(number_blues),let60=min(number_os),let90=max(number_os))
exp123_numeldf_sav_rav <- exp123_numeldf_sin_rav %>% group_by(exnum) %>% summarise (col60_mean=mean(col60),col90_mean=mean(col90),let60_mean=mean(let60),let90_mean=mean(let90),col60_sd=sd(col60),col90_sd=sd(col90),let60_sd=sd(let60),let90_sd=sd(let90))

#for experiment 4
exp4_numeldf_sin_rav <- exp4_raw  %>% filter(number_bo > 40)
exp4_numeldf_sin_rav <- exp4_numeldf_sin_rav %>% group_by(sub,task) %>% summarise (e60=min(number_bo),e90=max(number_bo))
exp4_numeldf_sav_rav <- exp4_numeldf_sin_rav %>% group_by(task) %>% summarise (e60_mean=mean(e60),e90_mean=mean(e90),e60_sd=sd(e60),e90_sd=sd(e90))

####2.2. Confidence as a function of difficulty and accuracy####
#three-way repeated measures ANOVA confidence as a function of correctness (correct vs error), task difficulty (easy vs hard) and task (color vs letter)
exp1_confcorer_sin_rav$taskf <- factor(exp1_confcorer_sin_rav$task)
exp1_confcorer_sin_rav$correctf <- factor(exp1_confcorer_sin_rav$correct)
exp1_confcorer_sin_rav$diff <- factor(exp1_confcorer_sin_rav$dif)
exp1_confcorer_sin_rav$subf <- as.character(paste("S",exp1_confcorer_sin_rav$sub))

summary(aov(conf ~ correctf * diff * taskf + Error(subf / (correctf * diff * taskf)), data = exp1_confcorer_sin_rav %>% filter(subf != "S 47.412" & subf != "S 52.3262"))) #we have to exclude two participants, since they did not have errors when the letter task was easy, and thus a category is empty

exp1_taskrep$taskf <- factor(exp1_taskrep$task)
exp1_taskrep$correctf <- factor(exp1_taskrep$correct)
exp1_taskrep$diff <- factor(exp1_taskrep$dif)
exp1_taskrep$subf <- as.character(paste("S",exp1_taskrep$sub))

summary(lmer(conf ~ correctf * diff * taskf + (1|subf) , data = exp1_taskrep %>% filter(subf != "S 47.412" & subf != "S 52.3262"))) 

####2.3. Priority effects from task difficulty in Experiment 1####
#percentage trials color task responded first to in EH vs HE 
exp1_prio_sin_rav %>% group_by (catperf2) %>% summarise (prob = mean(prob))

#paired t-test proportion trials color chosen first, EH vs HE
exp1_dfdifdif <- exp1_prio_sin_rav %>% ungroup() %>% select(sub,catperf2,prob) %>% filter(catperf2=="EH"|catperf2=="HE") %>% spread(catperf2,prob)
exp1sup_ttest1 <- t.test(exp1_dfdifdif$EH,exp1_dfdifdif$HE,paired=TRUE)
do_report_ttest(exp1sup_ttest1)

exp1_confhigherspread_samediff_correct_sin_rav <- exp1_confhigher_samediff_correct_sin_rav %>% group_by(sub,catperf2) %>% select(sub,catperf2,prob) %>% spread(catperf2,prob)
t.test(exp1_confhigherspread_samediff_correct_sin_rav$EE,exp1_confhigherspread_samediff_correct_sin_rav$HH,paired=TRUE)#proportions are not equal between EE and HH 

#one-tailed t-test difference of proportions EH - HE > 0 (priority effect)
t.test(exp1_priodif_sin_rav$difacc, mu=0, alternative="greater")

exp1_confhigher_EE_correct_sin_rav <- exp1_confhigher_samediff_correct_sin_rav %>% filter(catperf2=="EE")
t.test(exp1_confhigher_EE_correct_sin_rav$prob, mu=0.5, alternative="greater") #if we split data per trial type, EE is only marginally > 0.5

exp1_confhigher_HH_correct_sin_rav <- exp1_confhigher_samediff_correct_sin_rav %>% filter(catperf2=="HH")
t.test(exp1_confhigher_HH_correct_sin_rav$prob, mu=0.5, alternative="greater") #while HH is still significantly > 0.5

####4. Preregistered replication of Experiment 1####
#confidence ratings associated to first choice are bigger than those associated to second choice
exprep1_ttest1 <- t.test(exprep1_conffise_sin_rav$conffirst,exprep1_conffise_sin_rav$confsecond,paired=TRUE) 
do_report_ttest(exprep1_ttest1)

#t-test proportion trials where first choice had greater confidence than second choice
exprep1_ttest2 <- t.test(exprep1_confhigher_all_correct_sin_rav$prob, mu=0.5)#for all trial types
do_report_ttest(exprep1_ttest2)
exprep1_ttest3 <- t.test(exprep1_confhigher_samediffcollapsed_correct_sin_rav$prob, mu=0.5)#only for EE and HH, correct
do_report_ttest(exprep1_ttest3)
exprep1_ttest4 <- t.test(exprep1_confhigher_samediffcollapsed_wrong_sin_rav$prob, mu=0.5)#only for EE and HH, errors
do_report_ttest(exprep1_ttest4)

#logistic regression (priority as a function of expected difficulty, performance and confidence) and model comparison
exprep1_logistdf <- exprep1_raw %>% 
  mutate(difdifq = ifelse(catperf=="90-60",30,ifelse(catperf=="60-90",-30,0)), #30 = color easy, letter hard, -30 = color hard, letter easy, 0 both tasks same difficulty
         difcorrect = ifelse(correctcol==1 & correctlet==0,1, ifelse(correctcol==0 & correctlet==1,-1,0)), #1 = color task correct & letter incorrect, -1 = color task incorrect & letter correct, 0 = both either correct or incorrect
         difconf = hconf - vconf, #positive = confidence higher for color task
         subf = as.character(paste("S",sub)),
         choice1binf = as.factor(choice1bin), #choice1binf: 1 = color chosen, 0 = letter chosen
         difcorrect100 = difcorrect*100)#multiply performance by 100 so it is in the same scale as other variables  

exprep1_logistdf$choice1binf <- as.factor(exprep1_logistdf$choice1binf)
modfull_rep <- glmer(choice1binf ~ difdifq + difcorrect100 + difconf + (1|subf), data = exprep1_logistdf,family = binomial)
modpart_rep <- glmer(choice1binf ~ difdifq + difcorrect100 + (1|subf), data = exprep1_logistdf,family = binomial)
summary(modfull_rep)
summary(modpart_rep)
anova(modfull_rep,modpart_rep)

mod0_rep <- lmer(difconf ~ difdifq + difcorrect100 + (1|subf), data = exprep1_logistdf)
summary(glmer(choice1binf ~  resid(mod0_rep) + (1|subf), data = exprep1_logistdf,family = binomial, offset = predict(modpart_rep) ))

#summary(lmer(residuals(modpart) ~ residuals(mod0) + (1|as.factor(exprep1_logistdf$subf))))
#summary(lm(residuals(modpart) ~ residuals(mod0)))
#summary(glmer(choice1binf ~ predict(modpart) + difconf + (1|subf), data = exprep1_logistdf,family = binomial))
#summary(glmer(choice1binf ~  difconf + (1|subf), data = exprep1_logistdf,family = binomial, offset = predict(modpart) ))

#same logistic regression and model comparison as before, but only with different difficulty trial types
exprep1_logistdf_difdiff <- exprep1_raw %>% filter(catperf=="90-60" | catperf=="60-90") %>% 
  mutate(difdifq = ifelse(catperf=="90-60",30,-30), 
         difcorrect = ifelse(correctcol==1 & correctlet==0,1, ifelse(correctcol==0 & correctlet==1,-1,0)),
         difconf = hconf - vconf,
         subf = as.character(paste("S",sub)),
         choice1binf = as.factor(choice1bin),
         difcorrect100 = difcorrect*100)
exprep1_logistdf_difdiff$choice1binf <- as.factor(exprep1_logistdf_difdiff$choice1bin)

modfull_difdiff_rep <- glmer(choice1binf ~ difdifq + difcorrect100 + difconf + (1|subf), data = exprep1_logistdf_difdiff,family = binomial)
modpart_difdiff_rep <- glmer(choice1binf ~ difdifq + difcorrect100 + (1|subf), data = exprep1_logistdf_difdiff,family = binomial)
summary(modfull_difdiff_rep)
summary(modpart_difdiff_rep)
anova(modfull_difdiff_rep,modpart_difdiff_rep)

mod0_difdiff_rep <- lmer(difconf ~ difdifq + difcorrect100 + (1|subf), data = exprep1_logistdf_difdiff)
summary(glmer(choice1binf ~  resid(mod0_difdiff_rep) + (1|subf), data = exprep1_logistdf_difdiff,family = binomial, offset = predict(modpart_difdiff_rep) ))

#prioritizing high-confidence errors over low-confidence correct trials
exprep1_prierror %>% ungroup() %>% summarise(ntrials = sum(ntrials), nhits = sum(nhits)) #all trials pooled together
exprep1_prierror %>% filter (catperf == "60-60" | catperf == "90-90") %>% ungroup() %>% summarise(ntrials = sum(ntrials), nhits = sum(nhits)) # same difficulty trials pooled together

binom.test(nsuccesses, ntrials, 0.5, alternative="greater")#replace nsuccesses and ntrials by the output of the preceding analyses

#percentage trials color task responded first to in EH vs HE 
exprep1_prio_sin_rav %>% group_by (catperf2) %>% summarise (prob = mean(prob))

#one-tailed t-test difference of proportions EH - HE > 0 (priority effect)
exprep1_ttest4 <- t.test(exprep1_priodif_sin_rav$difacc, mu=0, alternative="greater")
do_report_ttest(exprep1_ttest4)

#reversal priority effect from difficulty for errors
exprep1_priosameaccdif_sav_rav <- exprep1_priosameaccdif_sin_rav %>% group_by(acccategory) %>%
  summarise(sd_difacc = sd(difacc, na.rm = TRUE),
            difacc = mean(difacc, na.rm = TRUE),
            n_difacc = n()) %>%
  mutate(se_difacc = sd_difacc / sqrt(n_difacc),
         low_ci_difacc = difacc - qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc,
         up_ci_difacc = difacc + qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc)

exprep1_priobothcorrectdif_sin_rav <- exprep1_priosameaccdif_sin_rav %>% filter(acccategory=="correct")
exprep1_priobotherrordif_sin_rav <- exprep1_priosameaccdif_sin_rav %>% filter(acccategory=="error")

exprep1_ttest2 <- t.test(exprep1_priobothcorrectdif_sin_rav$difacc, mu=0) #prioritization of easy tasks when both responses were correct
do_report_ttest(exprep1_ttest2)

exprep1_ttest3 <- t.test(exprep1_priobotherrordif_sin_rav$difacc, mu=0) #prioritization of hard tasks when both responses were errors
do_report_ttest(exprep1_ttest3)

exprep1_priosameaccdifspread_sin_rav <- exprep1_priosameaccdif_sin_rav %>% group_by(sub,acccategory) %>% select(sub,acccategory,difacc) %>% group_by(sub) %>% spread(acccategory,difacc)

exprep1_ttest4 <- t.test(exprep1_priosameaccdifspread_sin_rav$correct, exprep1_priosameaccdifspread_sin_rav$error, paired=TRUE) #the two last measures are different
do_report_ttest(exprep1_ttest4)

####5. Difference in observed accuracy between first and second choice, for experiments 1 to 3####
#Experiment 1, t-tests proportion accuracy 1st - 2nd choice, for all trial types pooled together
exp1sup_ttest50 <- t.test(exp1_acordtogetherall_sin_rav$difprob, mu=0)#proportion differences >0
do_report_ttest(exp1sup_ttest50)

#Experiment 1, t-tests proportion accuracy 1st - 2nd choice, for EE and HH trial types pooled together
exp1_acordsplit_sin_rav <- exp1_acordsplit_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH","EE"))
exp1sup_ttest51 <- t.test(exp1_acordsplitcollapsed_sin_rav$difprob, mu=0)#proportion differences >0
do_report_ttest(exp1sup_ttest51)

#Experiment 2, t-tests proportion accuracy 1st - 2nd choice, for all trial types pooled together
exp2sup_ttest50 <- t.test(exp2_acordtogetherall_sin_rav$difprob, mu=0)#proportion differences >0
do_report_ttest(exp2sup_ttest50)

#Experiment 2, t-tests proportion accuracy 1st - 2nd choice, for EE and HH trial types pooled together
exp2_acordsplit_sin_rav <- exp2_acordsplit_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH","EE"))
exp2sup_ttest51 <- t.test(exp2_acordsplitcollapsed_sin_rav$difprob, mu=0)#proportion differences >0
do_report_ttest(exp2sup_ttest51)

#Experiment 3, t-tests proportion accuracy 1st - 2nd choice, for all trial types pooled together
exp3sup_ttest50 <- t.test(exp3_acordtogetherall_sin_rav$difprob, mu=0)#proportion differences >0
do_report_ttest(exp3sup_ttest50)

#Experiment 3, t-tests proportion accuracy 1st - 2nd choice, for EE and HH trial types pooled together
exp3_acordsplit_sin_rav <- exp3_acordsplit_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH","EE"))
exp3sup_ttest51 <- t.test(exp3_acordsplitcollapsed_sin_rav$difprob, mu=0)#proportion differences >0
do_report_ttest(exp3sup_ttest51)

####6.1. Task-related bias ####
####relationship overall color/letter bias and priority effect####
exp1_priodif_sin_rav <- exp1_priodif_sin_rav %>% mutate(exptag="experiment 1")
exp2_priodif_sin_rav <- exp2_priodif_sin_rav %>% mutate(exptag="experiment 2")
exp3_priodif_sin_rav <- exp3_priodif_sin_rav %>% mutate(exptag="experiment 3")
exp123_priodif_sin_rav <- rbind(exp2_priodif_sin_rav,exp1_priodif_sin_rav,exp3_priodif_sin_rav)
exp123_priodif_sin_rav <- exp123_priodif_sin_rav %>% mutate(distavecomb = abs(0.5-avecomb))
cor.test(exp123_priodif_sin_rav$distavecomb,exp123_priodif_sin_rav$difacc)

exp2_bias_sin_rav <- exp2_bias_sin_rav %>% mutate(distto1= abs(1-ratio))
exp2_biasdistcol_sin_rav <- exp2_bias_sin_rav %>% filter(task == "color") %>% select(sub,experf,distto1) %>% spread(experf,distto1)
exp2_biasdistlet_sin_rav <- exp2_bias_sin_rav %>% filter(task == "letter") %>% select(sub,experf,distto1) %>% spread(experf,distto1)

t.test(exp2_biasdistcol_sin_rav$`60`, exp2_biasdistcol_sin_rav$`90`, paired=TRUE, alternative = "greater")
t.test(exp2_biasdistlet_sin_rav$`60`, exp2_biasdistlet_sin_rav$`90`, paired=TRUE, alternative = "greater")

####6.2. Position-related bias####
exp1_loc_sav_rav <- exp1_loc_sav_rav %>% mutate(exptag = "Experiment 1")
exp2_loc_sav_rav <- exp2_loc_sav_rav %>% mutate(exptag = "Experiment 2")
exp3_loc_sav_rav <- exp3_loc_sav_rav %>% mutate(exptag = "Experiment 3")
exp123_loc_sav_rav <- rbind(exp1_loc_sav_rav,exp2_loc_sav_rav,exp3_loc_sav_rav)

exp1_loc_sin_rav <- exp1_loc_sin_rav %>% mutate(exptag = "Experiment 1")
exp2_loc_sin_rav <- exp2_loc_sin_rav %>% mutate(exptag = "Experiment 2")
exp3_loc_sin_rav <- exp3_loc_sin_rav %>% mutate(exptag = "Experiment 3")
exp123_loc_sin_rav <- rbind(exp1_loc_sin_rav,exp2_loc_sin_rav,exp3_loc_sin_rav)

exp123_loc_sin_rav <- exp123_loc_sin_rav %>% mutate(locx=ifelse(locresp1==1|locresp1==3,"left","right"),locy=ifelse(locresp1<3,"up","down"))
exp123_loc_sin_rav$sub <- as.character(paste("S",exp123_loc_sin_rav$sub))
summary(aov(prop ~ locx*locy + Error(sub/(locx*locy)), data=exp123_loc_sin_rav %>% filter (exptag== "Experiment 1"))) #exp confidence
summary(aov(prop ~ locx*locy + Error(sub/(locx*locy)), data=exp123_loc_sin_rav %>% filter (exptag== "Experiment 2"))) #exp choice-only
summary(aov(prop ~ locx*locy + Error(sub/(locx*locy)), data=exp123_loc_sin_rav %>% filter (exptag== "Experiment 3"))) #exp delays

0.07822857/(0.07822857+0.08660893)#partial eta squared for vertical position in experiment confidence
0.06786/(0.06786+0.11254)#partial eta squared for vertical position in experiment choice-only
0.03223/(0.03223+0.14099)#partial eta squared for horizontal position in experiment delays
0.1084/(0.1084+0.1047)#partial eta squared for vertical position in experiment delays

exp4_loc_ttest1 <- t.test(exp4_chloc_pooled_sin_rav$prob, mu=0.5)#experiment prospective confidence
do_report_ttest(exp4_loc_ttest1)
exp5_loc_ttest1 <- t.test(exp5_loc_sin$prob, mu=0.5)#experiment mental calculation
do_report_ttest(exp5_loc_ttest1)

####7.1. Effect of an easy-first approach on performance: Experiments 1 to 5#### 
#experiments 1 to 3
exp1_difchoice_sav_rav <- exp1_difchoice_sav_rav %>% mutate(exptag="Experiment 1")
exp2_difchoice_sav_rav <- exp2_difchoice_sav_rav %>% mutate(exptag="Experiment 2")
exp3_difchoice_sav_rav <- exp3_difchoice_sav_rav %>% mutate(exptag="Experiment 3")
exp123_difchoice_sav_rav <- rbind(exp1_difchoice_sav_rav,exp2_difchoice_sav_rav,exp3_difchoice_sav_rav)

exp1_difchoice_sin_rav <- exp1_difchoice_sin_rav %>% mutate(exptag="Experiment 1")
exp2_difchoice_sin_rav <- exp2_difchoice_sin_rav %>% mutate(exptag="Experiment 2")
exp3_difchoice_sin_rav <- exp3_difchoice_sin_rav %>% mutate(exptag="Experiment 3")
exp123_difchoice_sin_rav <- rbind(exp1_difchoice_sin_rav,exp2_difchoice_sin_rav,exp3_difchoice_sin_rav)

exp123_difchoice_sin_rav$cheasy_firstf <- factor(exp123_difchoice_sin_rav$cheasy_first) #1 = easy task responded to first
exp123_difchoice_sin_rav$task_diff <- factor(exp123_difchoice_sin_rav$task_dif) #0 = easy, 1 = hard
exp123_difchoice_sin_rav$sub2 <- as.character(paste("S",exp123_difchoice_sin_rav$sub))

summary(aov(prob ~ cheasy_firstf * task_diff + Error(sub2 / (cheasy_firstf * task_diff)), data = exp123_difchoice_sin_rav %>% filter(exptag == "Experiment 1"))) #experiment confidence
summary(aov(prob ~ cheasy_firstf * task_diff + Error(sub2 / (cheasy_firstf * task_diff)), data = exp123_difchoice_sin_rav %>% filter(exptag == "Experiment 2"))) #experiment choice-only
summary(aov(prob ~ cheasy_firstf * task_diff + Error(sub2 / (cheasy_firstf * task_diff)), data = exp123_difchoice_sin_rav %>% filter(exptag == "Experiment 3"))) #experiment delay

2.1861225/(2.1861225+0.0537823)#eta squared task difficulty experiment confidence
1.3961/(1.3961+0.0551)#eta squared task difficulty experiment choice-only
2.5157/(2.5157+0.0799)#eta squared task difficulty experiment delay
0.01267/(0.01267+0.10265)#eta squared difficulty task chosen first experiment delay
0.07917570/(0.07917570+0.1237254)#eta squared interaction experiment confidence
0.02145/(0.02145+0.05511)#eta squared interaction experiment choice-only
0.00978/(0.00978+0.08045)#eta squared interaction experiment delay

#Experiment 4
exp4_difchoice_sin_rav$chosen1_diff <- factor(exp4_difchoice_sin_rav$chosen1_dif) #1 = easy task responded to first
exp4_difchoice_sin_rav$current_diff <- factor(exp4_difchoice_sin_rav$current_dif) #0 = easy, 1 = hard
exp4_difchoice_sin_rav$sub2 <- as.character(paste("S",exp4_difchoice_sin_rav$sub))

summary(aov(prob ~ chosen1_diff * current_diff + Error(sub2 / (chosen1_diff * current_diff)), data = exp4_difchoice_sin_rav))
2.5025/(2.5025+0.0812)#eta squared expected difficulty experiment prospective confidence

#Experiment 5
exp5_difrt_sin$dif2f <- factor(exp5_difrt_sin$dif2)#0 = easy, 1 = hard 
exp5_difrt_sin$easyquestfirstf <- factor(exp5_difrt_sin$easyquestfirst)#1 = easy task responded to first 
exp5_difrt_sin$subject2 <- as.character(paste("S",exp5_difrt_sin$subject))

aaaa <- exp5_prio_sin %>% select(subject,prob)
exp5_difrt_sin <- left_join(exp5_difrt_sin,aaaa)

summary(aov(rt ~ easyquestfirstf * dif2f + Error(subject2 / (easyquestfirstf * dif2f)), data = exp5_difrt_sin %>% filter(prob != 1))) #removing participants that chose only easy problems first, otherwise we are lacking the 4 categories for all participants
359.2/(359.2+1154.7)#eta squared problem difficulty experiment prospective confidence

####7.2. Supplementary experiment controlling response order ####
#three-way repeated measures ANOVA: performance as a function of difficulty task chosen first x expected difficulty x response condition (free vs imposed)
expsupp_difchoice_sav_rav <- expsupp_difchoice_sav_rav %>% mutate(respordbin2 = ifelse(respordbin==0,"Imposed response order","Free response order"))
expsupp_difchoice_sin_rav <- expsupp_difchoice_sin_rav %>% mutate(respordbin2 = ifelse(respordbin==0,"Imposed response order","Free response order"))
expsupp_difchoice_sin_rav <- expsupp_difchoice_sin_rav %>% mutate(sub_fact = factor(sub))
expsupp_difchoice_sin_rav$task_diff <- factor(expsupp_difchoice_sin_rav$task_dif)
expsupp_difchoice_sin_rav$cheasy_firstf <- factor(expsupp_difchoice_sin_rav$cheasy_first)
summary(aov(prob ~ respordbin2 * task_diff * cheasy_firstf + Error(sub_fact/(respordbin2 * task_diff * cheasy_firstf)), data = expsupp_difchoice_sin_rav))
2.4378/(2.4378+0.0593)#eta squared task difficulty experiment imposed order

#paired t-test proportion trials color chosen first, EH vs HE, in free order condition
expsupp_prio_sin_rav <- expsupp_prio_sin_rav %>% mutate(catperf2=ifelse(catperf=="90-60","EH","HE"))
expsupp_prio_sin_rav %>% group_by (catperf2) %>% summarise (prob = mean(prob)) #to know proportion trials color task chosen for each trial type
expsupp_dfdifdif <- expsupp_prio_sin_rav %>% ungroup() %>% select(sub,catperf2,prob) %>% spread(catperf2,prob)
eimp_ttest1 <- t.test(expsupp_dfdifdif$EH,expsupp_dfdifdif$HE,paired=TRUE)
do_report_ttest(eimp_ttest1)

####8. Reliability analyses for experiments 1 to 5####
#Exp 1, proportion higher confidence for 1st choice
exp1_confhigher_all_correct_splitrun_sin_rav <- exp1_raw %>% mutate(exphalf = ifelse(run<3,"firsthalf","secondhalf")) %>% group_by(sub,exphalf) %>% do(calculate_proportions(.,conffirsthigher))
exp1_confhigher_all_correct_splitrun_sin_rav <- exp1_confhigher_all_correct_splitrun_sin_rav %>% ungroup() %>% select(sub,exphalf,prob) %>% group_by(sub) %>% spread(exphalf,prob)
cor.test(exp1_confhigher_all_correct_splitrun_sin_rav$firsthalf,exp1_confhigher_all_correct_splitrun_sin_rav$secondhalf,paired=TRUE) 

#Exp 1, difference of proportions (EH - HE)
exp1_prio_splitrun_sin_rav <- exp1_raw %>% mutate(exphalf = ifelse(run<3,"firsthalf","secondhalf")) %>% group_by(sub,catperf,exphalf) %>% do(calculate_proportions(.,choice1bin))
exp1_prio_splitrun_sin_rav <- exp1_prio_splitrun_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
exp1_dfdifdif_splitrun <- exp1_prio_splitrun_sin_rav %>% ungroup() %>% select(sub,catperf2,exphalf,prob) %>% filter(catperf2=="EH"|catperf2=="HE") %>% group_by(exphalf) %>% spread(catperf2,prob) %>% mutate(difprops = EH - HE) %>% ungroup() %>% select(sub,exphalf,difprops) %>% spread(exphalf,difprops)
cor.test(exp1_dfdifdif_splitrun$firsthalf,exp1_dfdifdif_splitrun$secondhalf,paired=TRUE) 

#Exp 2, difference of proportions (EH - HE)
exp2_prio_splitrun_sin_rav <- exp2_raw %>% mutate(exphalf = ifelse(run<3,"firsthalf","secondhalf")) %>% group_by(sub,catperf,exphalf) %>% do(calculate_proportions(.,choice1bin))
exp2_prio_splitrun_sin_rav <- exp2_prio_splitrun_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
exp2_dfdifdif_splitrun <- exp2_prio_splitrun_sin_rav %>% ungroup() %>% select(sub,catperf2,exphalf,prob) %>% filter(catperf2=="EH"|catperf2=="HE") %>% group_by(exphalf) %>% spread(catperf2,prob) %>% mutate(difprops = EH - HE) %>% ungroup() %>% select(sub,exphalf,difprops) %>% spread(exphalf,difprops)
cor.test(exp2_dfdifdif_splitrun$firsthalf,exp2_dfdifdif_splitrun$secondhalf,paired=TRUE) 

#Exp 3, difference of proportions (EH - HE)
exp3_prio_splitrun_sin_rav <- exp3_raw %>% mutate(exphalf = ifelse(run<3,"firsthalf","secondhalf")) %>% group_by(sub,catperf,exphalf) %>% do(calculate_proportions(.,choice1bin))
exp3_prio_splitrun_sin_rav <- exp3_prio_splitrun_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
exp3_dfdifdif_splitrun <- exp3_prio_splitrun_sin_rav %>% ungroup() %>% select(sub,catperf2,exphalf,prob) %>% filter(catperf2=="EH"|catperf2=="HE") %>% group_by(exphalf) %>% spread(catperf2,prob) %>% mutate(difprops = EH - HE) %>% ungroup() %>% select(sub,exphalf,difprops) %>% spread(exphalf,difprops)
cor.test(exp3_dfdifdif_splitrun$firsthalf,exp3_dfdifdif_splitrun$secondhalf,paired=TRUE) 

#Exp 4, proportion blocks easy chosen first
exp4_chdif_collapsed_splitrun_sin_rav <- exp4_tcraw %>% mutate(exphalf = ifelse(run<3,"firsthalf","secondhalf")) %>% group_by(sub,exphalf) %>% do(calculate_proportions(.,choicedif)) %>% ungroup() %>% select(sub,exphalf,prob) %>% spread(exphalf,prob)
cor.test(exp4_chdif_collapsed_splitrun_sin_rav$firsthalf,exp4_chdif_collapsed_splitrun_sin_rav$secondhalf) 

#Exp 5, proportion trials easy problem chosen first
exp5_prio_splithalf_sin <- exp5_raw %>% group_by(subject) %>% mutate(trial = 1:n(), exphalf = ifelse(trial<11,"firsthalf","secondhalf")) %>% mutate(easyquestfirst2 = ifelse(easyquestfirst=="1",1,0)) %>% group_by(subject,exphalf) %>% do(calculate_proportions(.,easyquestfirst2)) %>% select(subject,exphalf,prob) %>% group_by(subject) %>% spread(exphalf,prob)
cor.test(exp5_prio_splithalf_sin$firsthalf,exp5_prio_splithalf_sin$secondhalf) 


###########################OLD ANALYSES#######################################


####MORE EXPERIMENT CHOICE-ONLY####
exp2_acordsplitall_sin_rav <- exp2_acordsplitall_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
t.test(exp2_acordsplitall_sin_rav$difprob[exp2_acordsplitall_sin_rav$catperf2=="EE"], mu=0, alternative="greater")#proportion differences >0
t.test(exp2_acordsplitall_sin_rav$difprob[exp2_acordsplitall_sin_rav$catperf2=="EH"], mu=0, alternative="greater")#proportion differences >0
t.test(exp2_acordsplitall_sin_rav$difprob[exp2_acordsplitall_sin_rav$catperf2=="HE"], mu=0, alternative="greater")#proportion differences >0
t.test(exp2_acordsplitall_sin_rav$difprob[exp2_acordsplitall_sin_rav$catperf2=="HH"], mu=0, alternative="greater")#proportion differences >0

exp2_acordsplitgathered_sin_rav <- exp2_acordsplit_sin_rav %>% group_by(sub,catperf2) %>% select(sub,catperf2,difprob) %>% spread(catperf2,difprob)
t.test(exp2_acordsplitgathered_sin_rav$EE,exp2_acordsplitgathered_sin_rav$HH,paired=TRUE)#proportion differences are equal between EE and HH 

