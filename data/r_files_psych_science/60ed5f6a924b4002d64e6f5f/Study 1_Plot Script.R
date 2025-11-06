if(!require(yarrr)){install.packages("yarrr")}
if(!require(haven)){install.packages("haven")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
  
setwd("/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Data for Sharing/Study 1/Data for Analyses & Scripts")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Reading in data from Study 1
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_s1 <- read_sav("Study 1_Data.sav") %>%
  filter (filter_dat==1,neg_feed>0,pos_feed>0) %>% # Applying data exclusion criteria
  mutate(Condition = ifelse(cond_rec==0,"control","experimental"),
         Self_view =ifelse(TSBI_bin=="low","negative","positive"))
  
data_s1_feed <- data_s1 %>%
  filter (filter_dat==1,neg_feed>0,pos_feed>0) %>% # Applying data exclusion criteria
  select(pseudo,pos_feed,neg_feed,TSBI_bin,cond_rec, pref_con,pref_inc)
  

# Converting to long data format
data_s1_long <- gather(data_s1_feed,neg_feed,pos_feed,key= "feedback_type", value="responses")

# Recoding variables 
data_s1_long <- data_s1_long %>%
  mutate(Feedback = ifelse(feedback_type=="neg_feed","negative","positive"),
         Condition = ifelse(cond_rec==0,"control","experimental"),
         Self_view =ifelse(TSBI_bin=="low","negative","positive"))
data_s1_long2 <- data_s1_long[order(data_s1_long$pseudo), ]

# Plotting the results 
pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plot1.pdf",width = 10,height =6 )

yarrr::pirateplot(formula = responses~Feedback+Self_view+Condition,
                  data = data_s1_long2,
                  xlab = "Conditions",
                  ylab = "Willingess to read in-depth feedback",
                  theme = 2, 
                  inf.f.o = .7, 
                  inf.b.o = .5,
                  point.o = .3, 
                  bar.f.o = .5, 
                  bean.f.o = .4,
                  bean.b.o = .5, 
                  avg.line.o = 0,
                  point.col = "black", 
                  bar.f.col = c("#5DADE2","#EC7063"),
                  inf.f.col = c("#5DADE2","#EC7063"),
                  inf.method = "withinci",
                  inf.within="pseudo",
                  plot = FALSE)

dev.off()

############### Using consistent & inconsistent feedback rather than positive and negative one #############

# Converting to long data format
data_s1_long2 <- gather(data_s1_feed,pref_con, pref_inc,key= "feedback_type", value="responses") 

# Recoding variables 
data_s1_long2 <- data_s1_long2 %>%
  mutate(Feedback = ifelse(feedback_type=="pref_inc","inconsistent","consistent"),
         Condition = ifelse(cond_rec==0,"control","experimental"))

# Plotting the results 
pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plot2.pdf",width = 5,height = 5.5)

yarrr::pirateplot(formula = responses~Feedback+Condition,
                  data = data_s1_long2,
                  xlab = "Conditions",
                  ylab = "Willingess to read in-depth feedback",
                  theme = 2, 
                  inf.f.o = .7, 
                  inf.b.o = .5,
                  point.o = .3, 
                  bar.f.o = .5, 
                  bean.f.o = .4,
                  bean.b.o = .5, 
                  avg.line.o = 0,
                  point.col = "black", 
                  bar.f.col = c("#AF7AC5","#AAB7B8"),
                  inf.f.col = c("#AF7AC5","#AAB7B8"),
                  inf.method = "se",
                  inf.within="pseudo")

dev.off()


############################# Plotting the results for selection variable  ################################

pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plot3.pdf",width = 5 ,height = 5.5)

yarrr::pirateplot(formula = choice~Self_view+Condition,
                  data = data_s1,
                  ylab = "Probability of selecting positive feedback",
                  theme = 4, 
                  inf.f.o = .7, 
                  inf.b.o = .5,
                  point.o = 0, 
                  bar.f.o = .5, 
                  bean.f.o = .4,
                  bean.b.o = .5, 
                  avg.line.o = 0,
                  bar.f.col = c("#5DADE2","#EC7063"),
                  inf.f.col = c("#5DADE2","#EC7063"),
                  inf.method = "se",
                  inf.within="pseudo")

dev.off()

###################################### For Supplementary Materials ########################################
############################# Plotting the results for selection variable  ################################

# ACCURACY
# Converting to long data format
data_s1_acc <- data_s1 %>% select(acc_inc, acc_con, cond_rec) %>%
  mutate(Condition = ifelse(cond_rec==0,"control","experimental"))
data_s1B_acc <- data_s1B %>% select(acc_inc, acc_con, con_rec) %>%
  mutate(Condition = ifelse(con_rec==0,"control","experimental"))
data_s2_acc <- data_s2 %>% select(acc_inc, acc_con, cond_rec)%>%
  mutate(Condition = ifelse(cond_rec==0,"control","experimental"))

data_s1_accuracy_long <- gather(data_s1_acc,acc_inc, acc_con, key= "feedback_type", value="accuracy") %>%
  mutate(study = "Study 1")
data_s1b_accuracy_long <- gather(data_s1B_acc,acc_inc, acc_con, key= "feedback_type", value="accuracy")%>%
  mutate(study = "Study 1B")
data_s2_accuracy_long <- gather(data_s2_acc,acc_inc, acc_con, key= "feedback_type", value="accuracy") %>%
  mutate(study = "Study 2")

data_all_accuracy_long <- bind_rows(data_s1_accuracy_long,data_s1b_accuracy_long,data_s2_accuracy_long)

# Recoding variables 
data_all_accuracy_long <- data_all_accuracy_long %>%
  mutate(Feedback = ifelse(feedback_type=="acc_inc","inconsistent","consistent"))

# Plotting the results 
pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plots/Revision/Accuracy.pdf",width = 15,height = 6.5)

yarrr::pirateplot(formula = accuracy~Feedback+Condition+study,
                  data = data_all_accuracy_long,
                  xlab = "Conditions",
                  ylab = "Subjective accuracy",
                  theme = 2, 
                  inf.f.o = .7, 
                  inf.b.o = .5,
                  point.o = .3, 
                  bar.f.o = .5, 
                  bean.f.o = .4,
                  bean.b.o = .5, 
                  avg.line.o = 0,
                  point.col = "black", 
                  bar.f.col = c("#138D75","#DE3163"),
                  inf.f.col = c("#138D75","#DE3163"),
                  inf.method = "se",
                  inf.within="pseudo")

dev.off()


# COMPETENCE
# Converting to long data format
data_s1_com <- data_s1 %>% select(comp_inc, comp_cons, cond_rec) %>%
  mutate(Condition = ifelse(cond_rec==0,"control","experimental"))
data_s1B_com <- data_s1B %>% select(comp_inco, comp_cons, con_rec) %>%
  mutate(Condition = ifelse(con_rec==0,"control","experimental"))
data_s2_com <- data_s2 %>% select(comp_inc, comp_con, cond_rec)%>%
  mutate(Condition = ifelse(cond_rec==0,"control","experimental"))

data_s1_competence_long <- gather(data_s1_com,comp_inc, comp_cons, key= "feedback_type", value="competence") %>%
  mutate(study = "Study 1")
data_s1b_competence_long <- gather(data_s1B_com,comp_inco, comp_cons, key= "feedback_type", value="competence")%>%
  mutate(study = "Study 1B")
data_s2_competence_long <- gather(data_s2_com, comp_inc, comp_con, key= "feedback_type", value="competence") %>%
  mutate(study = "Study 2")

data_all_competence_long <- bind_rows(data_s1_competence_long,data_s1b_competence_long,data_s2_competence_long)

# Recoding variables 
data_all_competence_long <- data_all_competence_long %>%
  mutate(Feedback = ifelse(feedback_type=="comp_inc" | feedback_type=="comp_inco","inconsistent","consistent"))

# Plotting the results 
pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plots/Revision/Competence.pdf",width = 15,height = 6.5)

yarrr::pirateplot(formula = competence~Feedback+Condition+study,
                  data = data_all_competence_long,
                  xlab = "Conditions",
                  ylab = "Perceived source comepetence",
                  theme = 2, 
                  inf.f.o = .7, 
                  inf.b.o = .5,
                  point.o = .3, 
                  bar.f.o = .5, 
                  bean.f.o = .4,
                  bean.b.o = .5, 
                  avg.line.o = 0,
                  point.col = "black", 
                  bar.f.col = c("#85929E","#D35400"),
                  inf.f.col = c("#85929E","#D35400"),
                  inf.method = "se",
                  inf.within="pseudo")

dev.off()




