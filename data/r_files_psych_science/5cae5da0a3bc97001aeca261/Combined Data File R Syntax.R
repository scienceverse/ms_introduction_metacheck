#Combined Adult & Child R Syntax

lib<-c("lme4","readr","tidyr","effects","ggplot2","psych","MASS",
       "Rmisc","plyr","dplyr","lmerTest","ggthemes","emmeans","pastecs")
lapply(lib,require,character.only=TRUE)

combined<-read.csv(file.choose()) #read in combined data file
View(combined_val)

###### examining  validation #########

combined_val<-combined %>% gather(stim,resp,8:9)
combined_val<- combined_val %>% separate(stim, into = c("fluff","stim","fluff2"),sep="_")
combined_val$fluff<-NULL
combined_val$fluff2<-NULL

combined_val$agebucket<-as.factor(combined_val$agebucket)
combined_val$agebucket<-revalue(combined_val$agebucket,c("1"="Younger","2"="Older","3"="Adults"))
combined_val<-combined_val %>%
  filter(agebucket!="NA")
combined_val_kid<-combined_val %>% filter (agebucket != "Adults")
combined_val_adults<-combined_val %>% filter (agebucket == "Adults")


mod_val_adults<-lmer(resp~stim + (1|SID),data=combined_val_adults)
summary(mod_val_adults)

adult_val_means<-emmeans(mod_combo_val,"stim")
adult_val_means


####### going wide to long for SCT measure ###########
combined_sct<-combined %>% gather(stim,resp,10:15)
combined_sct<- combined_sct %>% separate(stim, into = c("platform","stim","resp_type"),sep="_")
combined_sct$platform<-NULL
combined_sct$resp_type<-NULL
combined_sct<- combined_sct %>% separate(stim, into = c("t_race","t_gender"),sep=1)
combined_sct$t_race<-revalue(combined_sct$t_race,c("A"="Asian Target","B"="Black Target","W"="White Target"))
combined_sct$t_gender<-revalue(combined_sct$t_gender,c("F"="Female Target","M"="Male Target"))
combined_sct$t_race<-as.factor(combined_sct$t_race)
combined_sct$t_gender<-as.factor(combined_sct$t_gender)
combined_sct$agebucket<-as.factor(combined_sct$agebucket)
combined_sct$agebucket<-revalue(combined_sct$agebucket,c("1"="Younger","2"="Older","3"="Adults"))
combined_sct<-combined_sct %>%
  filter(agebucket!="NA")

combined_sct<-within(combined_sct, t_race<-relevel(t_race, ref = "Black Target"))
combined_sct<-within(combined_sct, t_race<-relevel(t_race, ref = "Asian Target"))
combined_sct<-within(combined_sct, t_gender<-relevel(t_gender, ref = "Male Target"))

#subsetting for kid data vs adult data
combined_sct_kid<-combined_sct %>% filter (agebucket != "Adults")
combined_sct_adults<-combined_sct %>% filter (agebucket == "Adults")

#########contrast coding data ##############
combined_sct_contr<-combined_sct
combined_sct_contr$t_gender<-as.character(combined_sct_contr$t_gender)
combined_sct_contr$t_race<-as.character(combined_sct_contr$t_race)
combined_sct_contr$t_gender[combined_sct_contr$t_gender == "Male Target"] <- -0.5
combined_sct_contr$t_gender[combined_sct_contr$t_gender == "Female Target"] <- 0.5
combined_sct_contr$t_race[combined_sct_contr$t_race == "Black Target"] <- -1
combined_sct_contr$t_race[combined_sct_contr$t_race == "White Target"] <- 0
combined_sct_contr$t_race[combined_sct_contr$t_race == "Asian Target"] <- 1

combined_sct_contr$t_gender<-as.numeric(combined_sct_contr$t_gender)
combined_sct_contr$t_race<-as.numeric(combined_sct_contr$t_race)

combined_sct_contr_kid<- combined_sct_contr %>% filter(agebucket != "Adults")
combined_sct_contr_adult<-combined_sct_contr %>% filter(agebucket == "Adults")


#### testing main interaction prediction for entire sample (kids & adults)##########
mod_combined_sct<-lmer(resp~t_race*t_gender + (1|SID),data=combined_sct_contr)
summary(mod_combined_sct) #significant two-way interaction

########moderation by age bucket for entire sample (of kids & adults)#########

combined_sct_contr$agebucket<-as.character(combined_sct_contr$agebucket)
combined_sct_contr$agebucket[combined_sct_contr$agebucket == "Younger"] <- -1
combined_sct_contr$agebucket[combined_sct_contr$agebucket == "Older"] <- 0
combined_sct_contr$agebucket[combined_sct_contr$agebucket == "Adults"] <- 1
combined_sct_contr$agebucket<-as.numeric(combined_sct_contr$agebucket)

mod_graph<-lmer(resp~t_race*t_gender*agebucket + (1|SID),data=combined_sct_contr)
summary(mod_graph) #significant three-way by age bucket

df_combined_age<-effect(term="t_race:t_gender*agebucket",
               xlevels=list(t_race=c(-1,0,1),
                            t_gender=c(-.5,.5),
                            agebucket=c(-1,0,1)), 
               mod=mod_graph)
df_combined_age <- as.data.frame(df_combined_age)

df_combined_age$t_race<-as.factor(df_combined_age$t_race)
df_combined_age$t_gender<-as.factor(df_combined_age$t_gender)
df_combined_age$agebucket<-as.factor(df_combined_age$agebucket)

df_combined_age$t_race<-revalue(df_combined_age$t_race,c("-1"="Black","0"="White","1"="Asian"))
df_combined_age$t_gender<-revalue(df_combined_age$t_gender,c("-0.5"="Male Target","0.5"="Female Target"))
df_combined_age$agebucket<-revalue(df_combined_age$agebucket,c("-1"="Younger Children","0"="Older Children","1"="Adults"))

df_combined_age<-within(df_combined_age, t_race<-relevel(t_race, ref = "White"))
df_combined_age<-within(df_combined_age, t_race<-relevel(t_race, ref = "Asian"))

############# graphing data for just kids ####################

combined_sct_contr_kid$age_c<-scale(combined_sct_contr_kid$age_exact,center=TRUE,scale=FALSE)
mod_kid<-lmer(resp~t_race*t_gender*age_c + (1|SID),data=combined_sct_contr_kid)
summary(mod_kid)

df_kid<-effect(term="t_race:t_gender*age_c",
               xlevels=list(t_race=c(-1,0,1),
                            t_gender=c(-.5,.5),
                            age_c=c(-2,-1,0,1,2)), 
               mod=mod_kid)
df_kid <- as.data.frame(df_kid)
df_kid$t_race<-as.factor(df_kid$t_race)
df_kid$t_gender<-as.factor(df_kid$t_gender)
df_kid$age_c<-as.factor(df_kid$age_c)

df_kid$t_race<-revalue(df_kid$t_race,c("-1"="Black Target","0"="White Target","1"="Asian Target"))
df_kid$t_gender<-revalue(df_kid$t_gender,c("-0.5"="Male Target","0.5"="Female Target"))
df_kid$age_c<-revalue(df_kid$age_c,c("-2"="4","-1"="5","0"="6","1"="7","2"="8"))
df_kid$age_c<-as.character(df_kid$age_c)
df_kid$age_c<-as.numeric(df_kid$age_c)

combined_sct_contr_kid<-within(combined_sct_contr_kid, t_gender<-relevel(t_gender, ref = "Male Target"))
df_kid<-within(df_kid, t_gender<-relevel(t_gender, ref = "Male Target"))
df_kid<-within(df_kid, t_race<-relevel(t_race, ref = "White Target"))
df_kid<-within(df_kid, t_race<-relevel(t_race, ref = "Asian Target"))


plot_kid<-ggplot(data = df_kid, 
                 aes(x = age_c, y = fit, linetype= t_gender))+
  geom_jitter(data=combined_sct_kid, aes(x=age_exact, y=resp, colour = t_gender,shape=t_gender),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
  geom_line(aes(group=t_gender))+ 
  #geom_smooth(method="glm",fullrange=TRUE)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=t_gender),alpha=0.3) +
  scale_fill_manual(values=c("#01a2d9","#014d64"))+
  scale_color_manual(values=c("#01a2d9","#014d64"))+
  scale_y_continuous(limits = c(700, 1400))+
  scale_x_continuous(breaks = c(3,4,5,6,7,8), 
                     labels = c("3","4", "5", "6", "7","8"))+
  facet_wrap(~t_race)+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.text.x=element_text(size=14)) +
  labs(y = "Categorization Speed",x= "Participant Age",
       linetype="Target Gender",fill="Target Gender",shape="Target Gender",colour="Target Gender") 

plot_kid

############# analyzing data for just adults ####################

##log transforming RT data
combined_sct_contr_adult$resp_log = log(combined_sct_contr_adult$resp)
mod_adults<-lmer(resp~t_race*t_gender + (1|SID),data=combined_sct_contr_adult)
summary(mod_adults)

mod_adults_log<-lmer(resp_log~t_race*t_gender + (1|SID),
                     data=combined_sct_contr_adult)
summary(mod_adults_log)

##breaking it down by target gender
combined_sct_adults<-within(combined_sct_adults,
                            t_race<-relevel(t_race, ref = "Black Target"))
  
mod_adults_femface<-lmer(resp~t_race + (1|SID),
                             data=subset(combined_sct_adults,
                                         t_gender=="Female Target"))
summary(mod_adults_femface)


##breaking it down by target race
mod_adults_blackfaces<-lmer(resp~t_gender + (1|SID),
                             data=subset(combined_sct_adults,
                                         t_race=="Black Target"))
summary(mod_adults_blackfaces)


mean_log_adults <- summarySEwithin(combined_sct_contr_adult, measurevar="resp_log", 
                               withinvars=c("t_race","t_gender"), 
                               betweenvars = NULL,
                               idvar="SID", na.rm=TRUE, conf.interval=.95)

mean_adults <- summarySEwithin(combined_sct_adults, measurevar="resp", 
                               withinvars=c("t_race","t_gender"), 
                               betweenvars = NULL,
                               idvar="SID", na.rm=TRUE, conf.interval=.95)
mean_adults<-within(mean_adults, t_gender<-relevel(t_gender, ref = "Male Target"))
mean_adults<-within(mean_adults, t_race<-relevel(t_race, ref = "White Target"))
mean_adults<-within(mean_adults, t_race<-relevel(t_race, ref = "Asian Target"))

plot_adult<-ggplot() +
  geom_jitter(data=combined_sct_adults, 
              aes(x=t_gender, y=resp,colour = factor(t_gender),shape=t_gender),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
  geom_point(data = mean_adults, 
             aes(x = t_gender, y = resp, colour = t_gender, shape=t_gender), 
             stat = "identity", size = 4) +
  geom_errorbar(data = mean_adults, width = .1, 
                aes(x = t_gender, ymin = resp-ci, ymax = resp+ci, colour = t_gender)) +
 facet_wrap(~t_race)+
  ylim(500,1550) +
 # scale_x_discrete(labels=c("Male ","Female"))+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_economist() +
  labs(y = "Speed of Categorization", x = "Target Gender",colour="Target Gender",shape="Target Gender") 

plot_adult  



####### combining adult and kid graphs ###############

ggarrange(plot_sct_dv,plot_adult,ncol=2, 
       common.legend = TRUE, legend = "bottom")



