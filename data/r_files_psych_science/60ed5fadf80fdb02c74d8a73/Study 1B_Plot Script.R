if(!require(yarrr)){install.packages("yarrr")}
if(!require(haven)){install.packages("haven")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyr)){install.packages("tidyr")}
  
  
setwd("/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Data for Sharing/Study 1B/Combined and analyses")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Reading in data from Study 1B
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_s1B <- read_sav("Study 1B_final data.sav") %>%
  filter (filter_guess==1) %>% # Applying data exclusion criteria
  mutate(Condition = ifelse(con_rec==0,"control","experimental"),
         Self_view =ifelse(TSBI_RESULT=="niski","negative","positive"))
  
data_s1B_feed <- data_s1B %>%
  select(ID_RESPONDENTA,pos_feed,neg_feed,Self_view,Condition,pref_con,pref_inc)
  

# Converting to long data format
data_s1B_long <- gather(data_s1B_feed,neg_feed,pos_feed,key= "feedback_type", value="responses")

# Recoding variables 
data_s1B_long <- data_s1B_long %>%
  mutate(Feedback = ifelse(feedback_type=="neg_feed","negative","positive"))

# Plotting the results 
pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plot4B.pdf",width = 10,height =6 )

yarrr::pirateplot(formula = responses~Feedback+Self_view+Condition,
                  data = data_s1B_long,
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
                  inf.method = "se",
                  inf.within="pseudo")

dev.off()

############### Using consistent & inconsistent feedback rather than positive and negative one #############

# Converting to long data format
data_s1B_long2 <- gather(data_s1B_feed,pref_con, pref_inc,key= "feedback_type", value="responses") 

# Recoding variables 
data_s1B_long2 <- data_s1B_long2 %>%
  mutate(Feedback = ifelse(feedback_type=="pref_inc","inconsistent","consistent"))

# Plotting the results 
pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plot5B.pdf",width = 5,height = 5.5)

yarrr::pirateplot(formula = responses~Feedback+Condition,
                  data = data_s1B_long2,
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


#############################3 Plotting the results for selection variable  ################################

pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plot6B.pdf",width = 5 ,height = 5.5)

yarrr::pirateplot(formula = f_choice~Self_view+Condition,
                  data = data_s1B,
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


