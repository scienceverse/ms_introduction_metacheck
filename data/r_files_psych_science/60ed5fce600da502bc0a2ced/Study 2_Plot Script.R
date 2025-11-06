if(!require(yarrr)){install.packages("yarrr")}
if(!require(haven)){install.packages("haven")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(interactions)){install.packages("interactions")}
if(!require(sjPlot)){install.packages("sjPlot")}
if(!require(effects)){install.packages("effects")}
  
  
setwd("/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Data for Sharing/Study 2/Datasets")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Reading in data from Study 2
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_s2 <- read_sav("Study 2_data.sav") %>%
  filter (filter_guess==1) %>% # Applying data exclusion criteria
  mutate(Condition = ifelse(condition_recoded==0,"control","experimental"),
         Self_view =ifelse(TSBI_bin=="low","negative","positive"))
  
data_s2_feed <- data_s2 %>%
  select(ID,pos_feed,neg_feed,Self_view,Condition,pref_con,pref_inc)
  

# Converting to long data format
data_s2_long <- gather(data_s2_feed,neg_feed,pos_feed,key= "feedback_type", value="responses")

# Recoding variables 
data_s2_long <- data_s2_long %>%
  mutate(Feedback = ifelse(feedback_type=="neg_feed","negative","positive"))

# Plotting the results 
pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plots/Revision/Plot7.pdf",width = 10,height =6 )

yarrr::pirateplot(formula = responses~Feedback+Self_view+Condition,
                  data = data_s2_long,
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
data_s2_long2 <- gather(data_s2_feed,pref_con, pref_inc,key= "feedback_type", value="responses") 

# Recoding variables 
data_s2_long2 <- data_s2_long2 %>%
  mutate(Feedback = ifelse(feedback_type=="pref_inc","inconsistent","consistent"))

# Plotting the results 
pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plots/Revision/Plot8.pdf",width = 5,height = 5.5)

yarrr::pirateplot(formula = responses~Feedback+Condition,
                  data = data_s2_long2,
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

pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plots/Revision/Plot9.pdf",width = 5 ,height = 5.5)

yarrr::pirateplot(formula = choice~Self_view+Condition,
                  data = data_s2,
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


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotting interactions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Plotting the results for preference difference variable 
# Fitting the regression model
mod_pref <- lm(pref_dif ~ Condition*EA_self, data = data_s2)
summary(mod_pref)

#Plotting the results 
pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plots/Revision/Plot10.pdf",width = 8 ,height = 5.5)

theme_set(theme_sjplot())

plot_model(mod_pref, 
           type = "int", 
           terms = c("Condition","EA_self"), 
           mdrt.values = "meansd",
           title = NULL,
           axis.title = c("Condition","Preference for consistent vs. inconsistent feedback"),
           legend.title = "Self-Epistemic Authority", 
           legend_style(pos = "right"))


dev.off()


# Plotting the results for the choice variable and Epistemic Authority of Psychologists
# Fitting the regression model
mod_choice <- lm(choice ~ Self_view*Condition*EA_psych, data = data_s2)
summary(mod_choice)

#Plotting the results 
pdf(file="/Users/ewa/OneDrive - Uniwersytet Jagielloński/Autoweryfikacja OPUS/Papers/Psych Science/Plots/Revision/Plot11.pdf",width =10, height = 6)

theme_set(theme_sjplot())

plot_model(mod_choice, 
           type = "int", 
           terms = c("Self_view","Condition","EA_psych"), 
           title ="Epistemic authority ascribed to psychologists",
           mdrt.values = "meansd",
           axis.title = c("Self-view","Probability of selecting positive feedback"),
           legend.title = "Condition", 
           colors = c("#7D3C98","#138D75"),
           legend_style(pos = "top"))


dev.off()


mod_choice2 <- lm(choice ~ esteem +Condition +EA_psych+esteem*Condition+esteem*EA_psych+Condition*EA_psych, data = data_s2)
summary(mod_choice2)

#Plotting the results 
theme_set(theme_sjplot())

plot_model(mod_choice2, 
           type = "pred", 
           terms = c("esteem","Condition","EA_psych"), 
           title ="Epistemic authority ascribed to psychologists",
           mdrt.values = "meansd",
           axis.title = c("Self-view","Probability of selecting positive feedback"),
           legend.title = "Condition", 
           colors = c("#7D3C98","#138D75","blue"),
           legend_style(pos = "top"))


dev.off()
