
#read in libraries
lib<-c("lme4","readr","tidyr","effects","ggplot2","psych","MASS",
       "Rmisc","plyr","dplyr","lmerTest","ggthemes","lsmeans","pastecs","ggpubr")
lapply(lib,require,character.only=TRUE)

rage<-read.csv(file.choose()) #read in RAGE_Datavyu file
rage.1<-rage[c(1,4:82)]
View(rage.1)
######## ANALYSES FOR VALIDATION BLOCKS FOR SCT -- (DATAVYU) #############
rage_val_dv <- rage.1 %>% gather(stim,resp,5:6)
colnames(rage_val_dv)[colnames(rage_val_dv)=="stim"] <- "Stimulus"

mod_val_dv<-lmer(resp~Stimulus + (1|SID),data=rage_val_dv)
summary(mod_val_dv)

rage_val_dv$Stimulus<-as.factor(rage_val_dv$Stimulus)

mean_val_dv <- summarySEwithin(rage_val_dv, measurevar="resp", 
                            withinvars="Stimulus", 
                            betweenvars = NULL,
                            idvar="SID", na.rm=TRUE, conf.interval=.95)

mean_val_dv$Stimulus<-revalue(mean_val_dv$Stimulus,c("DV_atypical_RT"="Non-Prototypical",
                                               "DV_typical_RT"="Prototypical"))
rage_val_dv$Stimulus<-revalue(rage_val_dv$Stimulus,c("DV_atypical_RT"="Non-Prototypical",
                                                     "DV_typical_RT"="Prototypical"))
 mean_val_dv
plot_val_dv<-ggplot() +
    geom_jitter(data=rage_val_dv, aes(x=Stimulus, y=resp,colour = factor(Stimulus),shape=Stimulus),
              stat="identity", size = 1.5, alpha = .8, height = 0, width = 0.25) +
  geom_point(data = mean_val_dv, 
             aes(x = factor(Stimulus), y = resp, colour = factor(Stimulus),shape=Stimulus), 
             stat = "identity", size = 5) +
  geom_errorbar(data = mean_val_dv, width = .1, 
                aes(x = factor(Stimulus), ymin = resp-ci, ymax = resp+ci, colour = factor(Stimulus))) +
  ylim(800,1750) +
  scale_x_discrete(labels=c("Non-Prototypical","Prototypical"))+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_economist() +
  labs(y = "Speed of Categorization", x = "Stimulus Type", colour ="Stimulus") 

plot_val_dv

######## ANALYSES FOR CAT SPEED RESULTS WITH DATAVYU ##########
rage_dv_sct<-rage.1 %>% gather(stim,resp,7:12)
rage_dv_sct<- rage_dv_sct %>% separate(stim, into = c("platform","stim","resp_type"),sep="_")
rage_dv_sct$platform<-NULL
rage_dv_sct$resp_type<-NULL
rage_dv_sct<- rage_dv_sct %>% separate(stim, into = c("t_race","t_gender"),sep=1)
rage_dv_sct$t_race<-revalue(rage_dv_sct$t_race,c("A"="Asian Target","B"="Black Target","W"="White Target"))
rage_dv_sct$t_gender<-revalue(rage_dv_sct$t_gender,c("F"="Female Target","M"="Male Target"))
rage_dv_sct_factor<-rage_dv_sct

rage_dv_sct$t_gender[rage_dv_sct$t_gender == "Male Target"] <- -0.5
rage_dv_sct$t_gender[rage_dv_sct$t_gender == "Female Target"] <- 0.5

rage_dv_sct$t_race[rage_dv_sct$t_race == "Black Target"] <- -1
rage_dv_sct$t_race[rage_dv_sct$t_race == "White Target"] <- 0
rage_dv_sct$t_race[rage_dv_sct$t_race == "Asian Target"] <- 1

rage_dv_sct$t_gender<-as.numeric(rage_dv_sct$t_gender)
rage_dv_sct$t_race<-as.numeric(rage_dv_sct$t_race)

#### testing main interaction prediction
mod_sct_dv<-lmer(resp~t_race*t_gender + (1|SID),data=rage_dv_sct)
summary(mod_sct_dv)

mod_sct_dv.1<-lmer(resp~t_race*t_gender + (1|SID),data=rage_dv_sct_factor)
emm_gen_sct<-emmeans(mod_sct_dv.1, "t_gender", by="t_race") #gender comparison by race
emm_race_sct<-emmeans(mod_sct_dv.1, "t_race", by="t_gender") # race comparison by gender
pairs(emm_race_sct)
pairs(emm_gender_sct)

mean_sct.dv <- summarySEwithin(rage_dv_sct_factor, measurevar="resp", 
                               withinvars=c("t_race","t_gender"), 
                               betweenvars = NULL,
                               idvar="SID", na.rm=TRUE, conf.interval=.95)
mean_sct.dv<-within(mean_sct.dv, t_gender<-relevel(t_gender, ref = "Male Target"))
mean_sct.dv<-within(mean_sct.dv, t_race<-relevel(t_race, ref = "White Target"))
mean_sct.dv<-within(mean_sct.dv, t_race<-relevel(t_race, ref = "Asian Target"))

rage_dv_sct_factor$t_gender<-as.factor(rage_dv_sct_factor$t_gender)
rage_dv_sct_factor$t_race<-as.factor(rage_dv_sct_factor$t_race)
rage_dv_sct_factor<-within(rage_dv_sct_factor, t_gender<-relevel(t_gender, ref = "Male Target"))
rage_dv_sct_factor<-within(rage_dv_sct_factor, t_race<-relevel(t_race, ref = "White Target"))
rage_dv_sct_factor<-within(rage_dv_sct_factor, t_race<-relevel(t_race, ref = "Asian Target"))

## graphing main result
plot_sct_dv<-ggplot() +
  geom_jitter(data=rage_dv_sct_factor, aes(x=t_gender, y=resp,colour = factor(t_gender),shape=t_gender),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
  geom_point(data = mean_sct.dv, 
             aes(x = t_gender, y = resp, colour = t_gender, shape=t_gender), 
             stat = "identity", size = 4) +
  geom_errorbar(data = mean_sct.dv, width = .1, 
                aes(x = t_gender, ymin = resp-ci, ymax = resp+ci, colour = t_gender)) +
  facet_wrap(~t_race)+
  ylim(500,1550) +
  scale_x_discrete(labels=c("Male ","Female"))+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_economist() +
  labs(y = "Speed of Categorization", x = "Target Gender",colour="Target Gender",shape="Target Gender") 

plot_sct_dv  

### filtering just for female targets to examine slopes

rage_dv_sct_femstim <-rage_dv_sct %>% filter(t_gender==.5)
rage_dv_sct_femstim$t_race<-as.factor(rage_dv_sct_femstim$t_race)
mod_sct_dv_femstim<-lmer(resp~t_race + (1|SID),data=rage_dv_sct_femstim)
summary(mod_sct_dv_femstim) #significant slope for race of target for female stim

rage_dv_sct_malestim <-rage_dv_sct %>% filter(t_gender==-.5)
mod_sct_dv_malestim<-lmer(resp~t_race + (1|SID),data=rage_dv_sct_malestim)
summary(mod_sct_dv_malestim) #ns slope for male stim

######## moderation by age? ##########
rage_dv_sct.2<-rage_dv_sct
rage_dv_sct.2$age_c<-scale(rage_dv_sct.2$age_exact,center=TRUE,scale=FALSE)
mod_sct_dv_age<-lmer(resp~t_race*t_gender*age_c + (1|SID),data=rage_dv_sct.2)
summary(mod_sct_dv_age) # marginal three-way interaction
confint(mod_sct_dv_age)

###testing just older kids
rage_dv_sct.3<-rage_dv_sct %>% filter (age_exact > 6)
mod_sct_dv_older<-lmer(resp~t_race*t_gender + (1|SID),data=rage_dv_sct.3)
summary(mod_sct_dv_older)

### testing just younger kids
rage_dv_sct.4<-rage_dv_sct %>% filter (age_exact < 6)
mod_sct_dv_younger<-lmer(resp~t_race*t_gender + (1|SID),data=rage_dv_sct.4)
summary(mod_sct_dv_younger)

######### graphing 3-way interaction between target race, target gender, and age of participant ##########
df_age<-effect(term="t_race:t_gender*age_c",
               xlevels=list(t_race=c(-1,0,1),
                            t_gender=c(-.5,.5),
                            age_c=c(-2,-1,0,1,2)), 
               mod=mod_sct_dv_age)
df_age <- as.data.frame(df_age)

df_age$t_race<-as.factor(df_age$t_race)
df_age$t_gender<-as.factor(df_age$t_gender)
rage_dv_sct.2$t_race<-as.factor(rage_dv_sct.2$t_race)
rage_dv_sct.2$t_gender<-as.factor(rage_dv_sct.2$t_gender)

df_age$t_race<-revalue(df_age$t_race,c("-1"="Black","0"="White","1"="Asian"))
rage_dv_sct.2$t_race<-revalue(rage_dv_sct.2$t_race,c("-1"="Black","0"="White","1"="Asian"))
df_age$t_gender<-revalue(df_age$t_gender,c("-0.5"="Male Target","0.5"="Female Target"))
rage_dv_sct.2$t_gender<-revalue(rage_dv_sct.2$t_gender,c("-0.5"="Male Target","0.5"="Female Target"))

df_age$age_c<-as.character(df_age$age_c)
rage_dv_sct.2$age_c<-as.character(rage_dv_sct.2$age_c)
df_age$age_c<-revalue(df_age$age_c,c("-2"="4","-1"="5","0"="6","1"="7","2"="8"))
df_age$age_c<-as.numeric(df_age$age_c)

df_age<-within(df_age, t_race<-relevel(t_race, ref = "White"))
df_age<-within(df_age, t_race<-relevel(t_race, ref = "Asian"))

plot_age<-ggplot(data = df_age, 
                 aes(x = age_c, y = fit, linetype= t_gender))+
  geom_jitter(data=rage_dv_sct.2, aes(x=age_exact, y=resp, colour = t_gender,shape=t_gender),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
  geom_line(aes(group=t_gender))+ 
  #geom_smooth(method="glm",fullrange=TRUE)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=t_gender),alpha=0.3) +
  scale_fill_manual(values=c("#01a2d9","#014d64"))+
  scale_color_manual(values=c("#01a2d9","#014d64"))+
  scale_y_continuous(limits = c(700, 1400))+
  scale_x_continuous(limits=c(4,8))+
  facet_wrap(~t_race)+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.text.x=element_text(size=14)) +
  labs(y = "Categorization Speed",x= "Participant Age",
       linetype="Target Gender",fill="Target Gender",shape="Target Gender",colour="Target Gender") 

plot_age

###########breakdown  3-way by target gender and/or target race ##############

mod_sct_dv_femstim_age<-lmer(resp~t_race*age_exact + (1|SID),data=rage_dv_sct_femstim)
summary(mod_sct_dv_femstim_age) #ns interaction
mod_sct_dv_malestim_age<-lmer(resp~t_race*age_exact + (1|SID),data=rage_dv_sct_malestim)
summary(mod_sct_dv_malestim_age) #ns interaction

mod_sct_dv_malestim_age<-lmer(resp~t_race*age_exact + (1|SID),data=rage_dv_sct_malestim)
summary(mod_sct_dv_malestim_age)
##break down marginal 3-way by target race

rage_dv_sct_blackstim<-rage_dv_sct %>% filter(t_race== -1)
rage_dv_sct_blackstim$age_c<-scale(rage_dv_sct_blackstim$age_exact,center=TRUE,scale=FALSE)
mod_sct_dv_blackstim_age<-lmer(resp~t_gender*age_c + (1|SID),data=rage_dv_sct_blackstim)
summary(mod_sct_dv_blackstim_age) #significant two-way interaction for Black targets

##what is the slope for black women by age?
rage_dv_sct_blackfemstim<-rage_dv_sct %>% filter(t_gender== .5 & t_race == -1)
rage_dv_sct_blackfemstim$age_c<-scale(rage_dv_sct_blackfemstim$age_exact,center=TRUE,scale=FALSE)
mod_sct_dv_blackfemstim_age<-lm(resp~age_c,data=rage_dv_sct_blackfemstim)
summary(mod_sct_dv_blackfemstim_age)#sig decrease

rage_dv_sct_whitestim<-rage_dv_sct %>% filter(t_race== 0)
rage_dv_sct_whitestim$age_c<-scale(rage_dv_sct_whitestim$age_exact,center=TRUE,scale=FALSE)
mod_sct_dv_whitestim_age<-lmer(resp~t_gender*age_c + (1|SID),data=rage_dv_sct_whitestim)
summary(mod_sct_dv_whitestim_age) #ns interaction for White targets p =.123

rage_dv_sct_asianstim<-rage_dv_sct %>% filter(t_race== 1)
mod_sct_dv_asianstim_age<-lmer(resp~t_gender*age_exact + (1|SID),data=rage_dv_sct_asianstim)
summary(mod_sct_dv_asianstim_age) #ns for asian targets

############## TESTING FOR INTERACTIONS WITH KID GENDER AND KID ETHNICITY --  DATAVYU #############

rage_dv_sct.2$Kid_Gender<-as.character(rage_dv_sct.2$Kid_Gender)
rage_dv_sct.2$Kid_Eth<-as.character(rage_dv_sct.2$Kid_Eth)

rage_dv_sct.2$Kid_Gender[rage_dv_sct.2$Kid_Gender == "Male"] <- -0.5
rage_dv_sct.2$Kid_Gender[rage_dv_sct.2$Kid_Gender == "Female"] <- 0.5
rage_dv_sct.2$Kid_Eth[rage_dv_sct.2$Kid_Eth == "Black"] <- -1
rage_dv_sct.2$Kid_Eth[rage_dv_sct.2$Kid_Eth == "White"] <- 0
rage_dv_sct.2$Kid_Eth[rage_dv_sct.2$Kid_Eth == "Asian"] <- 1

rage_dv_sct.2$Kid_Gender<-as.numeric(rage_dv_sct.2$Kid_Gender)
rage_dv_sct.2$Kid_Eth<-as.numeric(rage_dv_sct.2$Kid_Eth)

### models testing for interactions with participant characteristics
mod_sct_dv.2<-lmer(resp~t_race*t_gender*Kid_Gender + (1|SID),data=rage_dv_sct.2)
summary(mod_sct_dv.2)  # no 3-way interaction with kid gender

mod_sct_dv.3<-lmer(resp~t_race*t_gender*Kid_Eth + (1|SID),data=rage_dv_sct.2)
summary(mod_sct_dv.3) # no 3-way interaction with kid ethnicity

mod_sct_dv.4<-lmer(resp~t_race*t_gender*Kid_Gender*Kid_Eth + (1|SID),data=rage_dv_sct.2)
summary(mod_sct_dv.4) #no 4-way interaction


######### moderation by contact? looking at moderation by some environmental variables ##########
##peer group variables
rage_dv_sct.2$wb_frienddiff<-rage_dv_sct.2$Friends_White-rage_dv_sct.2$Friends_Black
rage_dv_sct.2$wb_frienddiff_c<-scale(rage_dv_sct.2$wb_frienddiff,center=TRUE,scale=FALSE)
rage_dv_sct.2$bfriend_c<-scale(rage_dv_sct.2$Friends_Black,center=TRUE,scale=FALSE)

#zip code variables
rage_dv_sct.2$wb_zipdiff<-rage_dv_sct.2$Zip_Perc_White-rage_dv_sct.2$Zip_Perc_Black
rage_dv_sct.2$wb_zipdiff_c<-scale(rage_dv_sct.2$wb_zipdiff,center=TRUE,scale=FALSE)
rage_dv_sct.2$zipblack_c<-scale(rage_dv_sct.2$Zip_Perc_Black,center=TRUE,scale=FALSE)

#caregiver variables
rage_dv_sct.2$bbs_c<-scale(rage_dv_sct.2$BBS_BlackLatino,center=TRUE,scale=FALSE)
rage_dv_sct.2$b_expo<-scale(rage_dv_sct.2$Total_Black_Exposure,center=TRUE,scale=FALSE)

mod_bfriends<-lmer(resp~t_race*t_gender*bfriend_c + (1|SID),data=rage_dv_sct.2)
summary(mod_bfriends) #no moderation by number of Black friends 

mod_zip_black<-lmer(resp~ t_race*t_gender*zipblack_c + (1|SID),data=rage_dv_sct.2)
summary(mod_zip_black) #no moderation by percentage of neighborhood that is black
mod_zip_wbdiff<-lmer(resp~ t_race*t_gender*wb_zipdiff_c + (1|SID),data=rage_dv_sct.2)
summary(mod_zip_wbdiff) #no moderation by W/B zip diff 

mod_minority_bbs<-lmer(resp~ t_race*t_gender*bbs_c + (1|SID),data=rage_dv_sct.2)
summary(mod_minority_bbs) #no moderation by contact with black/latino babysitter
mod_black_expo<-lmer(resp~ t_race*t_gender*b_expo + (1|SID),data=rage_dv_sct.2)
summary(mod_black_expo) #no moderation by overall contact with black people

####### moderation by parent variables? nope ################
#-- political ideology
rage_dv_sct.2$lib_ideo<-scale(rage_dv_sct.2$LibIdeo,center=TRUE,scale=FALSE)
rage_dv_sct.2$cons_ideo<-scale(rage_dv_sct.2$ConsIdeo,center=TRUE,scale=FALSE)

mod_lib<-lmer(resp~ t_race*t_gender*lib_ideo + (1|SID),data=rage_dv_sct.2)
summary(mod_lib) #no interaction
mod_cons<-lmer(resp~ t_race*t_gender*cons_ideo + (1|SID),data=rage_dv_sct.2)
summary(mod_cons) #no interaction

#colorblind or multicultural ideology
rage_dv_sct.2$cb_c<-scale(rage_dv_sct.2$CB_Overall,center=TRUE,scale=FALSE)
rage_dv_sct.2$mc_c<-scale(rage_dv_sct.2$MC_Overall,center=TRUE,scale=FALSE)
rage_dv_sct.2$mc_cb_diff<-rage_dv_sct.2$mc_c-rage_dv_sct.2$cb_c
#moderation by colorblindness
mod_cb<-lmer(resp~ t_race*t_gender*cb_c + (1|SID),data=rage_dv_sct.2)
summary(mod_cb) #no interaction

#moderation by multiculturalism
mod_mc<-lmer(resp~ t_race*t_gender*mc_c + (1|SID),data=rage_dv_sct.2)
summary(mod_mc) #no interaction





######### ANALYSES FOR ERROR RATES -- DATAVYU ########

rage_mistake<-rage.1 %>% gather(stim,resp,21:26)
rage_mistake<- rage_mistake %>% separate(stim, into = c("platform","stim","DV"),sep="_")
rage_mistake<- rage_mistake %>% separate(stim, into = c("t_race","t_gender"),sep=1)
rage_mistake$DV<-NULL
rage_mistake$platform<-NULL
rage_mistake$t_race<-as.factor(rage_mistake$t_race)
rage_mistake$t_gender<-as.factor(rage_mistake$t_gender)

rage_mistake$t_race<-revalue(rage_mistake$t_race,c("A"="Asian Target","B"="Black Target","W"="White Target"))
rage_mistake$t_gender<-revalue(rage_mistake$t_gender,c("F"="Female Target","M"="Male Target"))
rage_mistake$age_c<-scale(rage_mistake$age_exact,center=TRUE,scale=FALSE)
rage_mistake$age_c<-as.numeric(rage_mistake$age_c)

## testing for error rates
mod_error<-glmer.nb(resp~t_race*t_gender*age_c + (1|SID),data=rage_mistake)
summary(mod_error)

emm_mistake<-emmeans(mod_error,"t_gender",by="t_race")
emm_mistake
pairs(emm_mistake)
emm_mistake.1<-emmeans(mod_error,"t_race",by="t_gender")
pairs(emm_mistake.1)

mean_mistake <- summarySEwithin(rage_mistake, measurevar="resp", 
                                withinvars=c("t_race","t_gender"), 
                                betweenvars = NULL,
                                idvar="SID", na.rm=TRUE, conf.interval=.95)
mean_mistake<-within(mean_mistake, t_race<-relevel(t_race, ref = "Asian Target"))
mean_mistake<-within(mean_mistake, t_gender<-relevel(t_gender, ref = "Male Target"))
rage_mistake<-within(rage_mistake, t_race<-relevel(t_race, ref = "White Target"))
rage_mistake<-within(rage_mistake, t_race<-relevel(t_race, ref = "Asian Target"))
rage_mistake<-within(rage_mistake, t_gender<-relevel(t_gender, ref = "Male Target"))
mean_mistake

plot_mistake<-ggplot() +
  geom_jitter(data=rage_mistake, aes(x=t_gender, y=resp,colour = factor(t_gender),shape=t_gender),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
    geom_point(data = mean_mistake, 
             aes(x = t_gender, y = resp, colour = t_gender,shape=t_gender), 
             stat = "identity", size = 5) +
  geom_errorbar(data = mean_mistake, width = .1, 
                aes(x = factor(t_gender), ymin = resp-ci, ymax = resp+ci, colour = factor(t_gender))) +
  ylim(0,4) +
  scale_x_discrete(labels=c("Male","Female"))+
  facet_wrap(~t_race)+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.text.x = element_text(size=14)) +
  scale_color_economist() +
  labs(y = "Number of Categorization Errors", x = "Target Gender", colour ="Target Gender",shape="Target Gender") 

plot_mistake  

mod_error_eth<-glmer.nb(resp~t_race*t_gender*Kid_Eth + (1|SID),data=rage_mistake)
summary(mod_error_eth)

mean_mistake_eth <- summarySEwithin(rage_mistake, measurevar="resp", 
                                withinvars=c("t_race","t_gender"), 
                                betweenvars = "Kid_Eth",
                                idvar="SID", na.rm=TRUE, conf.interval=.95)
mean_mistake_eth
mean_mistake_eth<-within(mean_mistake_eth, t_race<-relevel(t_race, ref = "White Target"))
mean_mistake_eth<-within(mean_mistake_eth, t_race<-relevel(t_race, ref = "Asian Target"))
mean_mistake_eth<-within(mean_mistake_eth, t_gender<-relevel(t_gender, ref = "Male Target"))
mean_mistake_eth<-within(mean_mistake_eth, Kid_Eth<-relevel(Kid_Eth, ref = "Asian"))
rage_mistake<-within(rage_mistake, Kid_Eth<-relevel(Kid_Eth, ref = "White"))
rage_mistake<-within(rage_mistake, Kid_Eth<-relevel(Kid_Eth, ref = "Asian"))


plot_mistake_eth<-ggplot() +
  geom_jitter(data=rage_mistake, aes(x=t_race, y=resp,colour = factor(t_gender),shape=t_gender),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
    geom_point(data = mean_mistake_eth, 
             aes(x = t_race, y = resp, colour = t_gender,shape=t_gender), 
             stat = "identity", size = 5) +
  geom_errorbar(data = mean_mistake_eth, width = .1, 
                aes(x = factor(t_race), ymin = resp-ci, ymax = resp+ci, colour = factor(t_gender))) +
  ylim(-.25,3) +
  #scale_x_discrete(labels=c("Asian","White","Black"))+
  facet_wrap(~Kid_Eth)+
  theme_few() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        strip.text.x = element_text(size=14)) +
  scale_color_economist() +
  labs(y = "Number of Categorization Errors", x = "Target Race", colour ="Target Gender",shape="Target Gender") 

plot_mistake_eth  

############ TESTING ERRORS DV -- CONTRAST CODES  - SUBSAMPLE (DATAVYU) ##############

rage_mistake_contr<-rage_mistake
rage_mistake_contr$t_gender<-as.character(rage_mistake_contr$t_gender)
rage_mistake_contr$t_race<-as.character(rage_mistake_contr$t_race)

rage_mistake_contr$t_gender[rage_mistake_contr$t_gender == "Male Target"] <- -0.5
rage_mistake_contr$t_gender[rage_mistake_contr$t_gender == "Female Target"] <- 0.5
rage_mistake_contr$t_race[rage_mistake_contr$t_race == "Black Target"] <- -1
rage_mistake_contr$t_race[rage_mistake_contr$t_race == "White Target"] <- 0
rage_mistake_contr$t_race[rage_mistake_contr$t_race == "Asian Target"] <- 1

rage_mistake_contr$t_gender<-as.numeric(rage_mistake_contr$t_gender)
rage_mistake_contr$t_race<-as.numeric(rage_mistake_contr$t_race)

rage_mistake_contr$age_c<-scale(rage_mistake_contr$age_exact,center=TRUE,scale=FALSE)
mistake_mod_contr<-glmer.nb(resp~t_race*t_gender*age_c + (1|SID),data=rage_mistake_contr)
summary(mistake_mod_contr)

        ########### testing for interactions with kid gen and kid eth for error rates -- CONTRAST CODES ###############

rage_mistake_contr$Kid_Eth<-as.character(rage_mistake_contr$Kid_Eth)
rage_mistake_contr$Kid_Eth[rage_mistake_contr$Kid_Eth == "Black"] <- -1
rage_mistake_contr$Kid_Eth[rage_mistake_contr$Kid_Eth == "White"] <- 0
rage_mistake_contr$Kid_Eth[rage_mistake_contr$Kid_Eth == "Asian"] <- 1
rage_mistake_contr$Kid_Eth<-as.numeric(rage_mistake_contr$Kid_Eth)

rage_mistake_contr$Kid_Gender<-as.character(rage_mistake_contr$Kid_Gender)
rage_mistake_contr$Kid_Gender[rage_mistake_contr$Kid_Gender == "Male"] <- -0.5
rage_mistake_contr$Kid_Gender[rage_mistake_contr$Kid_Gender == "Female"] <- 0.5
rage_mistake_contr$Kid_Gender<-as.numeric(rage_mistake_contr$Kid_Gender)

mistake_mod_contr_gen<-glmer.nb(resp~t_race*t_gender*Kid_Gender + (1|SID),data=rage_mistake_contr)
summary(mistake_mod_contr_gen)

mistake_mod_contr_eth<-glmer.nb(resp~t_race*t_gender*Kid_Eth + (1|SID),data=rage_mistake_contr)
summary(mistake_mod_contr_eth) #yes - three way interaction

rage_mistake_contr_black<- rage_mistake_contr %>% filter(Kid_Eth == -1)
rage_mistake_contr_white<- rage_mistake_contr %>% filter(Kid_Eth == 0)
rage_mistake_contr_asian<- rage_mistake_contr %>% filter(Kid_Eth == 1)

mod_error_black<-glmer.nb(resp~t_race*t_gender + (1|SID),data=rage_mistake_contr_black)
summary(mod_error_black)

mod_error_white<-glmer.nb(resp~t_race*t_gender + (1|SID),data=rage_mistake_contr_white)
summary(mod_error_white)

mod_error_asian<-glmer.nb(resp~t_race*t_gender + (1|SID),data=rage_mistake_contr_asian)
summary(mod_error_asian)

mistake_mod_contr_genxeth<-glmer.nb(resp~t_race*t_gender*Kid_Gender*Kid_Eth + (1|SID),data=rage_mistake_contr)
summary(mistake_mod_contr_genxeth) #no 4-way interation

#moderation by age?
rage_mistake_contr$age_c<-scale(rage_mistake_contr$age_exact,center=TRUE,scale=FALSE)
mod_error_3wayage<-glmer.nb(resp~t_race*t_gender*age_c + (1|SID),data=rage_mistake_contr)
summary(mod_error_3wayage)

        ########### Error rates recoded ########
rage_mistake_recode<-rage.1
rage_mistake_recode$ownrace_female_mistake<-ifelse(
    rage_mistake_recode$Kid_Eth=="Black",rage_mistake_recode$DV_BF_error,
      ifelse(rage_mistake_recode$Kid_Eth=="White",rage_mistake_recode$DV_WF_error,
      ifelse(rage_mistake_recode$Kid_Eth=="Asian",rage_mistake_recode$DV_AF_error,"WHAT")))
rage_mistake_recode$ownrace_male_mistake<-ifelse(
  rage_mistake_recode$Kid_Eth=="Black",rage_mistake_recode$DV_BM_error,
  ifelse(rage_mistake_recode$Kid_Eth=="White",rage_mistake_recode$DV_WM_error,
         ifelse(rage_mistake_recode$Kid_Eth=="Asian",rage_mistake_recode$DV_AM_error,"WHAT")))

rage_mistake_recode$diffrace_female_mistake<-ifelse(
  rage_mistake_recode$Kid_Eth=="Black",((rage_mistake_recode$DV_AF_error+rage_mistake_recode$DV_WF_error)/2),
      ifelse(rage_mistake_recode$Kid_Eth=="White",((rage_mistake_recode$DV_AF_error+rage_mistake_recode$DV_BF_error)/2),
             ifelse(rage_mistake_recode$Kid_Eth=="Asian",((rage_mistake_recode$DV_BF_error+rage_mistake_recode$DV_WF_error)/2),
                                                                 "WHAT")))
rage_mistake_recode$diffrace_male_mistake<-ifelse(
  rage_mistake_recode$Kid_Eth=="Black",((rage_mistake_recode$DV_AM_error+rage_mistake_recode$DV_WM_error)/2),
  ifelse(rage_mistake_recode$Kid_Eth=="White",((rage_mistake_recode$DV_AM_error+rage_mistake_recode$DV_BM_error)/2),
         ifelse(rage_mistake_recode$Kid_Eth=="Asian",((rage_mistake_recode$DV_BM_error+rage_mistake_recode$DV_WM_error)/2),
                "WHAT")))

rage_mistake_recode.1<-rage_mistake_recode %>% gather(stim,resp,81:84)
rage_mistake_recode.1<- rage_mistake_recode.1 %>% separate(stim, into = c("own","t_gender","mistake"),sep="_")
rage_mistake_recode.1$mistake<-NULL
rage_mistake_recode.1$own<-as.factor(rage_mistake_recode.1$own)
rage_mistake_recode.1$t_gender<-as.factor(rage_mistake_recode.1$t_gender)
rage_mistake_recode.1$resp<-as.numeric(rage_mistake_recode.1$resp)

View(rage_mistake_recode.1)
mod_mistake_recode<-glmer.nb(resp~own*t_gender + (1|SID),data=rage_mistake_recode.1)
summary(mod_mistake_recode)

rage_mistake$t_race<-revalue(rage_mistake$t_race,c("A"="Asian Target","B"="Black Target","W"="White Target"))
rage_mistake$t_gender<-revalue(rage_mistake$t_gender,c("F"="Female Target","M"="Male Target"))
rage_mistake$age_c<-scale(rage_mistake$age_exact,center=TRUE,scale=FALSE)




########## CORRELATIONS BETWEEN PREDICTORS AND REACTION TIME/CATEGORIZATION ERRORS ############
View(rage)
rage_enviro_corr_rt<-rage[c(1,4,9:14,27:34,37:39)]
rage_enviro_corr_mistakes<-rage[c(1,4,21:34,37:39)]

rage_parent_corr_rt<-rage[c(1,4,9:14,36:37,41:43)]
rage_parent_corr_mistakes<-rage[c(1,4,21:26,36:37,41:43)]

#printing the correlation matrix
rage_corr.1<-corr.test(rage_enviro_corr_rt)
rage_corr.1
rage_corr.2<-corr.test(rage_enviro_corr_mistakes)
rage_corr.2
rage_corr_parent<-corr.test(rage_parent_corr_rt)
rage_corr_parent
rage_corr_parent_mistake<-corr.test(rage_parent_corr_mistakes)
rage_corr_parent_mistake


############## stereotype data #############
View(rage_stereo)
rage_stereo<-rage.1[c(1:4,56:80)]
rage_stereo<-rage_stereo %>% filter(stereotypedata==1)
rage_stereo.1<-rage_stereo %>% gather(stim,resp,c(6:29))
rage_stereo.1<- rage_stereo.1 %>% separate(stim, into = c("valence","trait","target"),sep="_")
rage_stereo.1<- rage_stereo.1 %>% separate(target, into = c("t_race","t_gender"),sep=1)

rage_stereo.1$t_race<-revalue(rage_stereo.1$t_race,c("b"="Black","w"="White","a"="Asian"))
rage_stereo.1$t_gender<-revalue(rage_stereo.1$t_gender,c("m"="Man","f"="Woman"))
rage_stereo.1$valence<-revalue(rage_stereo.1$valence,c("pos"="Positive","neg"="Negative"))

rage_stereo.1$t_race<-as.factor(rage_stereo.1$t_race)
rage_stereo.1$t_gender<-as.factor(rage_stereo.1$t_gender)
rage_stereo.1$valence<-as.factor(rage_stereo.1$valence)

########### masculine stereotypes ###############
rage_stereo_masc <- rage_stereo.1 %>% 
  filter(trait == "masc" & t_gender == "Man")

mod_stereo_masc<-lmer(resp~t_race*valence + (1|SID),data=rage_stereo_masc)
summary(mod_stereo_masc)

#pulling and testing the marginal means
emm_stereo_masc<-emmeans(mod_stereo_masc,"t_race",by="valence")
pairs(emm_stereo_masc)
emm_stereo_masc.1<-emmeans(mod_stereo_masc,"valence",by="t_race")
pairs(emm_stereo_masc.1)


mean_stereo_masc <- summarySEwithin(rage_stereo_masc, measurevar="resp", 
                               withinvars=c("t_race","valence"), 
                               betweenvars = NULL,
                               idvar="SID", na.rm=TRUE, conf.interval=.95)
mean_stereo_masc<-within(mean_stereo_masc, t_race<-relevel(t_race,ref="White"))
mean_stereo_masc<-within(mean_stereo_masc, t_race<-relevel(t_race,ref="Asian"))
rage_stereo_masc<-within(rage_stereo_masc, t_race<-relevel(t_race,ref="White"))
rage_stereo_masc<-within(rage_stereo_masc, t_race<-relevel(t_race,ref="Asian"))

plot_stereo_masc<-ggplot() +
  geom_jitter(data=rage_stereo_masc, aes(x=t_race, y=resp,colour=t_race,shape=t_race),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
    geom_point(data = mean_stereo_masc, 
             aes(x = t_race, y = resp, colour=t_race,shape=t_race), stat = "identity", size = 5) +
  geom_errorbar(data = mean_stereo_masc, width = .1, 
                aes(x = factor(t_race), ymin = resp-ci, ymax = resp+ci, colour=t_race,shape=t_race)) +
  ylim(0,1) +
  geom_hline(yintercept = 0.1666)+
  facet_wrap(~valence)+
  scale_x_discrete(labels=c("Asian", "White","Black"))+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.text.x=element_text(size=14)) +
  scale_color_economist() +
  labs(y = "Proportion Choosing Target", x = "Target Race",colour="Target Race",shape="Target Race",
       title="Masculine Stereotypes")

plot_stereo_masc  

#contrast coding
rage_stereo_masc_contr<-rage_stereo_masc
rage_stereo_masc_contr$t_race<-as.character(rage_stereo_masc_contr$t_race)
rage_stereo_masc_contr$t_race[rage_stereo_masc_contr$t_race == "Black"] <- -1
rage_stereo_masc_contr$t_race[rage_stereo_masc_contr$t_race == "White"] <- 0
rage_stereo_masc_contr$t_race[rage_stereo_masc_contr$t_race == "Asian"] <- 1
rage_stereo_masc_contr$t_race<-as.numeric(rage_stereo_masc_contr$t_race)

rage_stereo_masc_contr$valence<-as.character(rage_stereo_masc_contr$valence)
rage_stereo_masc_contr$valence[rage_stereo_masc_contr$valence == "Positive"] <- 0.5
rage_stereo_masc_contr$valence[rage_stereo_masc_contr$valence == "Negative"] <- -0.5
rage_stereo_masc_contr$valence<-as.numeric(rage_stereo_masc_contr$valence)

mod_masc_stereo_contr<-lmer(resp~valence*t_race + (1|SID),data=rage_stereo_masc_contr)
summary(mod_masc_stereo_contr)

mod_masc_stereo_contr_age<-lmer(resp~valence*t_race*age_exact + (1|SID),data=rage_stereo_masc_contr)
summary(mod_masc_stereo_contr_age)


####### feminine stereotypes ##########
rage_stereo_fem <- rage_stereo.1 %>% 
  filter(trait == "fem" & t_gender == "Woman")

mod_stereo_fem<-lmer(resp~t_race*valence + (1|SID),data=rage_stereo_fem)
summary(mod_stereo_fem)

emm_stereo_fem<-emmeans(mod_stereo_fem,"t_race",by="valence")
emm_stereo_fem
pairs(emm_stereo_fem)

mean_stereo_fem <- summarySEwithin(rage_stereo_fem, measurevar="resp", 
                               withinvars=c("t_race","valence"), 
                               betweenvars = NULL,
                               idvar="SID", na.rm=TRUE, conf.interval=.95)
mean_stereo_fem<-within(mean_stereo_fem, t_race<-relevel(t_race,ref="White"))
mean_stereo_fem<-within(mean_stereo_fem, t_race<-relevel(t_race,ref="Asian"))
rage_stereo_fem<-within(rage_stereo_fem, t_race<-relevel(t_race,ref="White"))
rage_stereo_fem<-within(rage_stereo_fem, t_race<-relevel(t_race,ref="Asian"))


marginal2=lsmeans(mod_stereo_fem,pairwise~t_race:valence,
                 adjust="bonferroni")
marginal2$contrasts


plot_stereo_fem<-ggplot() +
  geom_jitter(data=rage_stereo_fem, aes(x=t_race, y=resp,colour=t_race,shape=t_race),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
  geom_point(data = mean_stereo_fem, 
             aes(x = t_race, y = resp, colour = t_race,shape=t_race), stat = "identity", size = 5) +
  geom_errorbar(data = mean_stereo_fem, width = .1, 
                aes(x = factor(t_race), ymin = resp-ci, ymax = resp+ci, colour = factor(t_race),shape=t_race)) +
  ylim(0,1) +
  facet_wrap(~valence)+
  geom_hline(yintercept = 0.1666)+
  scale_x_discrete(labels=c("Asian", "White","Black"))+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.text.x=element_text(size=14)) +
  scale_color_economist() +
  labs(y = "Proportion Choosing Target", x = "Target Race", title="Feminine Stereotypes")

plot_stereo_fem  

#contrast coding
rage_stereo_fem_contr<-rage_stereo_fem
rage_stereo_fem_contr$t_race<-as.character(rage_stereo_fem_contr$t_race)
rage_stereo_fem_contr$t_race[rage_stereo_fem_contr$t_race == "Black"] <- -1
rage_stereo_fem_contr$t_race[rage_stereo_fem_contr$t_race == "White"] <- 0
rage_stereo_fem_contr$t_race[rage_stereo_fem_contr$t_race == "Asian"] <- 1
rage_stereo_fem_contr$t_race<-as.numeric(rage_stereo_fem_contr$t_race)

rage_stereo_fem_contr$valence<-as.character(rage_stereo_fem_contr$valence)
rage_stereo_fem_contr$valence[rage_stereo_fem_contr$valence == "Positive"] <- 0.5
rage_stereo_fem_contr$valence[rage_stereo_fem_contr$valence == "Negative"] <- -0.5
rage_stereo_fem_contr$valence<-as.numeric(rage_stereo_fem_contr$valence)

mod_fem_stereo_contr<-lmer(resp~valence*t_race + (1|SID),data=rage_stereo_fem_contr)
summary(mod_fem_stereo_contr)
mod_fem_stereo_contr_age<-lmer(resp~valence*t_race*age_exact + (1|SID),data=rage_stereo_fem_contr)
summary(mod_fem_stereo_contr_age)


########### do stereotypes moderate the reaction time patterns? #############

View(rage_dv_sct)
rage_dv_sct$bm_stereo<-(rage_dv_sct$neg_masc_bm + rage_dv_sct$pos_masc_bm)/2
rage_dv_sct$wm_stereo<-(rage_dv_sct$neg_masc_wm + rage_dv_sct$pos_masc_wm)/2
rage_dv_sct$am_stereo<-(rage_dv_sct$neg_masc_am + rage_dv_sct$pos_masc_am)/2
rage_dv_sct$wb_stereo_diff<-rage_dv_sct$wm_stereo-rage_dv_sct$bm_stereo #difference in masculine stereotype endorsement for white vs. black
rage_dv_sct$wb_negmasc_stereo_diff<-rage_dv_sct$neg_masc_wm-rage_dv_sct$neg_masc_bm
rage_dv_sct$ab_stereo_diff<-rage_dv_sct$am_stereo-rage_dv_sct$bm_stereo

mod_rt_stereo<-lmer(resp~t_race*t_gender*bm_stereo + (1|SID),data=rage_dv_sct)
summary(mod_rt_stereo)
mod_rt_ab_stereo<-lmer(resp~t_race*t_gender*ab_stereo_diff + (1|SID),data=rage_dv_sct)
summary(mod_rt_ab_stereo)
mod_rt_stereo_diff<-lmer(resp~t_race*t_gender*wb_stereo_diff + (1|SID),data=rage_dv_sct)
summary(mod_rt_stereo_diff) #marginal interaction = .10


df_stereodiff<-effect(term="t_race:t_gender*wb_stereo_diff",
                  xlevels=list(t_race=c(-1,0,1),
                               t_gender=c(-.5,.5),
                               wb_stereo_diff=c(-.5,0,.5)), 
                  mod=mod_rt_stereo_diff)
df_stereodiff <- as.data.frame(df_stereodiff)
df_stereodiff$t_gender<-as.factor(df_stereodiff$t_gender)
df_stereodiff

plot_stereodiff<-ggplot(data = df_stereodiff, 
                    aes(x = wb_stereo_diff, y = fit, linetype= t_gender))+
  geom_line(aes(group=t_gender))+ 
  #geom_smooth(method="glm",fullrange=TRUE)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=t_gender),alpha=0.3,extendrange=TRUE) +
  scale_y_continuous(limits = c(750, 1250))+
  scale_x_continuous(limits=c(-.5,.5))+
  # geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
  facet_wrap(~t_race)+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_economist() +
  labs(y = "Categorization Speed", x = "White vs. Black Masculine Stereotype Difference",
       linetype="Target Gender",fill="Target Gender") 

plot_stereodiff

######## do gendered race stereos moderate main patterns? ############
# no, not really
#composite of choices of black men and asian women for masc and fem stereotypes, respectively
rage_dv_sct$genracestereo<-(rage_dv_sct$bm_stereo+rage_dv_sct$af_stereo)/2

rage_dv_sct$af_stereo<-(rage_dv_sct$neg_fem_af + rage_dv_sct$pos_fem_af)/2
rage_dv_sct$bf_stereo<-(rage_dv_sct$neg_fem_bf + rage_dv_sct$pos_fem_bf)/2
rage_dv_sct$wf_stereo<-(rage_dv_sct$neg_fem_wf + rage_dv_sct$pos_fem_wf)/2
rage_dv_sct$wb_femstereo_diff<-rage_dv_sct$wf_stereo-rage_dv_sct$bf_stereo
rage_dv_sct$ab_femstereo_diff<-rage_dv_sct$af_stereo-rage_dv_sct$bf_stereo

rage_dv_sct$wb_posfem_stereo_diff<-rage_dv_sct$pos_fem_wf-rage_dv_sct$pos_fem_bf
rage_dv_sct$bw_negfem_stereo_diff<-rage_dv_sct$neg_fem_bf-rage_dv_sct$neg_fem_wf
rage_dv_sct$ab_posfem_stereo_diff<-rage_dv_sct$pos_fem_af-rage_dv_sct$pos_fem_bf
rage_dv_sct$ab_negfem_stereo_diff<-rage_dv_sct$neg_fem_af-rage_dv_sct$neg_fem_bf


mod_rt_posfem_stereo<-lmer(resp~t_race*t_gender*ab_posfem_stereo_diff + (1|SID),data=rage_dv_sct)
summary(mod_rt_posfem_stereo)
mod_rt_abfemstereo_diff<-lmer(resp~t_race*t_gender*ab_femstereo_diff + (1|SID),data=rage_dv_sct)
summary(mod_rt_abfemstereo_diff)

mod_rt_negfem_stereo<-lmer(resp~t_race*t_gender*ab_negfem_stereo_diff + (1|SID),data=rage_dv_sct)
summary(mod_rt_negfem_stereo)

df_stereo<-effect(term="t_race:t_gender*ab_negfem_stereo_diff",
               xlevels=list(t_race=c(-1,0,1),
                            t_gender=c(-.5,.5),
                            ab_negfem_stereo_diff=c(-.5,0,.5)), 
               mod=mod_rt_negfem_stereo)
df_stereo <- as.data.frame(df_stereo)
df_stereo$t_gender<-as.factor(df_stereo$t_gender)

plot_stereo<-ggplot(data = df_stereo, 
                 aes(x = ab_negfem_stereo_diff, y = fit, linetype= t_gender))+
  geom_line(aes(group=t_gender))+ 
  #geom_smooth(method="glm",fullrange=TRUE)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=t_gender),alpha=0.3,extendrange=TRUE) +
  scale_y_continuous(limits = c(750, 1250))+
  scale_x_continuous(limits=c(-.5,.5))+
  # geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
  facet_wrap(~t_race)+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_economist() +
  labs(y = "Categorization Speed", x = "Asian (vs. Black) Neg Fem Prototype",linetype="Target Gender",fill="Target Gender") 

plot_stereo


mod_rt_genrace_stereo<-lmer(resp~t_race*t_gender*genracestereo + (1|SID),data=rage_dv_sct)
summary(mod_rt_genrace_stereo)



rage_mistake_contr$genracestereo<-(rage_mistake_contr$bm_stereo+rage_mistake_contr$af_stereo)/2

rage_mistake_contr$af_stereo<-(rage_mistake_contr$neg_fem_af + rage_mistake_contr$pos_fem_af)/2
rage_mistake_contr$bf_stereo<-(rage_mistake_contr$neg_fem_bf + rage_mistake_contr$pos_fem_bf)/2
rage_mistake_contr$wf_stereo<-(rage_mistake_contr$neg_fem_wf + rage_mistake_contr$pos_fem_wf)/2
rage_mistake_contr$wb_femstereo_diff<-rage_mistake_contr$wf_stereo-rage_mistake_contr$bf_stereo
rage_mistake_contr$ab_femstereo_diff<-rage_mistake_contr$af_stereo-rage_mistake_contr$bf_stereo

rage_mistake_contr$wb_posfem_stereo_diff<-rage_mistake_contr$pos_fem_wf-rage_mistake_contr$pos_fem_bf
rage_mistake_contr$wb_negfem_stereo_diff<-rage_mistake_contr$neg_fem_wf-rage_mistake_contr$neg_fem_bf
rage_mistake_contr$ab_posfem_stereo_diff<-rage_mistake_contr$pos_fem_af-rage_mistake_contr$pos_fem_bf
rage_mistake_contr$ab_negfem_stereo_diff<-rage_mistake_contr$neg_fem_af-rage_mistake_contr$neg_fem_bf


mod_mistake_posfem_stereo<-lmer(resp~t_race*t_gender*ab_posfem_stereo_diff + (1|SID),data=rage_mistake_contr)
summary(mod_mistake_posfem_stereo)
mod_mistake_negfem_stereo<-lmer(resp~t_race*t_gender*ab_negfem_stereo_diff + (1|SID),data=rage_mistake_contr)
summary(mod_mistake_negfem_stereo)

mod_mistake_abfemstereo_diff<-lmer(resp~t_race*t_gender*ab_femstereo_diff + (1|SID),data=rage_mistake_contr)
summary(mod_mistake_abfemstereo_diff)


####### do stereotypes correlate with RTs? #######
View(rage)
stereo_rt_corr<-rage[c(9:14,58:60,64:66,73:75,79:81)]
stereo_rt_corr$af_stereo<-(stereo_rt_corr$neg_fem_af + stereo_rt_corr$pos_fem_af)/2
stereo_rt_corr$bf_stereo<-(stereo_rt_corr$neg_fem_bf + stereo_rt_corr$pos_fem_bf)/2
stereo_rt_corr$wf_stereo<-(stereo_rt_corr$neg_fem_wf + stereo_rt_corr$pos_fem_wf)/2
stereo_rt_corr$wb_femstereo_diff<-stereo_rt_corr$wf_stereo-stereo_rt_corr$bf_stereo
stereo_rt_corr$ab_femstereo_diff<-stereo_rt_corr$af_stereo-stereo_rt_corr$bf_stereo

stereo_rt_corr.1<-stereo_rt_corr[c(1:6,31:35)]

stereo_corr_test.1<-corr.test(stereo_rt_corr.1)
stereo_corr_test.1

stereo_mistake_corr<-rage[c(21:26,57:80)]
stereo_mistake_corr$af_stereo<-(stereo_mistake_corr$neg_fem_af + stereo_mistake_corr$pos_fem_af)/2
stereo_mistake_corr$bf_stereo<-(stereo_mistake_corr$neg_fem_bf + stereo_mistake_corr$pos_fem_bf)/2
stereo_mistake_corr$wf_stereo<-(stereo_mistake_corr$neg_fem_wf + stereo_mistake_corr$pos_fem_wf)/2
stereo_mistake_corr$wb_femstereo_diff<-stereo_mistake_corr$wf_stereo-stereo_mistake_corr$bf_stereo
stereo_mistake_corr$ab_femstereo_diff<-stereo_mistake_corr$af_stereo-stereo_mistake_corr$bf_stereo
View(stereo_mistake_corr)
stereo_mistake_corr.1<-stereo_mistake_corr[c(1:9,13:15,22:24,28:35)]
stereo_mistake_corr_test<-corr.test(stereo_mistake_corr.1)
stereo_mistake_corr_test

