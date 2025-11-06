
#Read in libraries

lib<-c("lme4","readr","tidyr","effects","ggplot2","psych","MASS",
       "Rmisc","plyr","dplyr","lmerTest","ggthemes","emmeans","pastecs","ggpubr")
lapply(lib,require,character.only=TRUE)

rage_omni <- read.csv(file.choose())
rage_omni.1<-rage_omni
rage_omni.1$Study[which(rage_omni.1$Study==1)] <- -1  #monoracial study
rage_omni.1$Study[which(rage_omni.1$Study==2)] <- 1 #biracial vs. White comparison Study

rage_1<-rage_omni.1 %>% filter(Study == -1) #first monoracial study
rage_2<-rage_omni.1 %>% filter(Study == 1) #second biracial v White monoracial study

summary(rage_omni.1)
###### analyzing validation trials #########
rage_omni_val <- rage_omni.1 %>% gather(stim,resp,8:9)
colnames(rage_omni_val)[colnames(rage_omni_val)=="stim"] <- "Stimulus"

#main analyses
mod_val<-lmer(resp~Stimulus + (1|SID),data=rage_omni_val)
summary(mod_val)

#checking to make sure it doesn't interact by Study
mod_val_check<-lmer(resp~Stimulus*Study + (1|SID),data=rage_omni_val)
summary(mod_val_check) #main effect of Study (monoracial kids were slower), but no interaction

########## analysis for critical speed blocks #############
rage_omni_sct <- rage_omni.1 %>% gather(stim,resp,10:15)
rage_omni_sct<- rage_omni_sct %>% separate(stim, into = c("fluff","stim","fluff2"),sep="_")
rage_omni_sct$fluff<-NULL
rage_omni_sct$fluff2<-NULL
rage_omni_sct<- rage_omni_sct %>% separate(stim, into = c("t_race","t_gender"),sep=1)
rage_omni_sct$t_race<-revalue(rage_omni_sct$t_race,c("A"="Asian","B"="Black","W"="White"))
rage_omni_sct$t_gender<-revalue(rage_omni_sct$t_gender,c("F"="Female Target","M"="Male Target"))

#### creating duplicate df for plotting later 
rage_omni_sct_factor<-rage_omni_sct
rage_omni_sct_factor$t_race<-as.factor(rage_omni_sct_factor$t_race)
rage_omni_sct_factor$t_gender<-as.factor(rage_omni_sct_factor$t_gender)

#### recoding factors as numeric contrasts
rage_omni_sct$t_gender[rage_omni_sct$t_gender == "Male Target"] <- -0.5
rage_omni_sct$t_gender[rage_omni_sct$t_gender == "Female Target"] <- 0.5

rage_omni_sct$t_race[rage_omni_sct$t_race == "Black"] <- -1
rage_omni_sct$t_race[rage_omni_sct$t_race == "White"] <- 0
rage_omni_sct$t_race[rage_omni_sct$t_race == "Asian"] <- 1

rage_omni_sct$t_gender<-as.numeric(rage_omni_sct$t_gender)
rage_omni_sct$t_race<-as.numeric(rage_omni_sct$t_race)

#### testing for main race x gender interaction
mod_sct<-lmer(resp~t_race*t_gender + (1|SID),data=rage_omni_sct)
summary(mod_sct)

#making sure it doesn't interact with Study
mod_sct_check<-lmer(resp~t_race*t_gender*Study + (1|SID),data=rage_omni_sct)
summary(mod_sct_check) # no higher order interaction by Study

#testing for specific race comparisons
mod_sct.1<-lmer(resp~t_race*t_gender + (1|SID),data=rage_omni_sct_factor)
summary(mod_sct.1)

emm_gen_sct<-emmeans(mod_sct.1, "t_gender", by="t_race") #gender comparison by race
pairs(emm_gen_sct)
emm_race_sct<-emmeans(mod_sct.1, "t_race", by="t_gender") #race comparison by gender
emm_race_sct
pairs(emm_race_sct)
### calling main race x gender table
mean_sct <- summarySEwithin(rage_omni_sct_factor, measurevar="resp", 
                            withinvars=c("t_race","t_gender"), 
                            betweenvars = NULL,
                            idvar="SID", na.rm=TRUE, conf.interval=.95)
mean_sct<-within(mean_sct, t_race<-relevel(t_race, ref = "White"))
mean_sct<-within(mean_sct, t_race<-relevel(t_race, ref = "Asian"))
mean_sct<-within(mean_sct, t_gender<-relevel(t_gender, ref = "Male Target"))
mean_sct


####### looking at interaction by age #########
  #centering age variable
rage_omni_sct$age_c<-scale(rage_omni_sct$age,center=TRUE,scale=FALSE)
rage_omni_sct$age_c<-as.numeric(rage_omni_sct$age_c)

rage_omni_sct_factor$age_c<-scale(rage_omni_sct_factor$age,
                                  center=TRUE,scale=FALSE)
rage_omni_sct_factor$age_c<-as.numeric(rage_omni_sct_factor$age_c)
  # running model for testing age interaction
mod_sct_age<-lmer(resp~t_race*t_gender*age_c + (1|SID),data=rage_omni_sct)
summary(mod_sct_age) # no higher order interactions by Study

#testing to make sure it doesn't interact by Study
mod_sct_age_check<-lmer(resp~t_race*t_gender*age_c*Study + (1|SID),data=rage_omni_sct)
summary(mod_sct_age_check)

######## graphing age interaction #########

df_age<-effect(term="t_race:t_gender*age_c",
               xlevels=list(t_race=c(-1,0,1),
                            t_gender=c(-.5,.5),
                            age_c=c(-2,-1,0,1,2)), 
               mod=mod_sct_age)
df_age <- as.data.frame(df_age)

df_age$t_race<-as.factor(df_age$t_race)
df_age$t_gender<-as.factor(df_age$t_gender)
df_age$t_race<-revalue(df_age$t_race,c("-1"="Black","0"="White","1"="Asian"))
df_age$t_gender<-revalue(df_age$t_gender,c("-0.5"="Male Target","0.5"="Female Target"))

df_age$age_c<-as.character(df_age$age_c)
df_age$age_c<-revalue(df_age$age_c,c("-2"="4","-1"="5","0"="6","1"="7","2"="8"))
df_age$age_c<-as.numeric(df_age$age_c)

df_age<-within(df_age, t_race<-relevel(t_race, ref = "White"))
df_age<-within(df_age, t_race<-relevel(t_race, ref = "Asian"))
df_age<-within(df_age, t_gender<-relevel(t_gender, ref = "Male Target"))
rage_omni_sct_factor<-within(rage_omni_sct_factor, 
                             t_gender<-relevel(t_gender, ref = "Male Target"))

### graph code
plot_age<-ggplot(data = df_age, 
                 aes(x = age_c, y = fit, linetype= t_gender))+
  geom_jitter(data=rage_omni_sct_factor, 
              aes(x=age_exact, y=resp, colour = t_gender,shape=t_gender),
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

      ##### age interaction plot with target race as moderator and target gender as facet wrap #######
rage_omni_sct_factor<-within(rage_omni_sct_factor, t_race<-relevel(t_race, ref = "White"))
rage_omni_sct_factor<-within(rage_omni_sct_factor, t_race<-relevel(t_race, ref = "Asian"))

plot_age2<-ggplot(data = df_age, 
                 aes(x = age_c, y = fit, linetype= t_race))+
  geom_jitter(data=rage_omni_sct_factor, 
              aes(x=age, y=resp, colour = t_race,shape=t_race),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
  geom_line(aes(group=t_race))+ 
  #geom_smooth(method="glm",fullrange=TRUE)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=t_race),alpha=0.3) +
  #scale_fill_manual(values=c("#01a2d9","#014d64"))+
  #scale_color_manual(values=c("#01a2d9","#014d64"))+
  scale_y_continuous(limits = c(700, 1400))+
  scale_x_continuous(limits=c(4,8))+
  facet_wrap(~t_gender)+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.text.x=element_text(size=14)) +
  labs(y = "Categorization Speed",x= "Participant Age",
       linetype="Target Race",fill="Target Race",shape="Target Race",colour="Target Race") 

plot_age2


      ##### breaking down age interaction by target gender #######
rage_omni_sct_femtarget<-rage_omni_sct %>% filter (t_gender == 0.5)
rage_omni_sct_maletarget<-rage_omni_sct %>% filter (t_gender == -0.5)

rage_omni_sct_fem_factor<-rage_omni_sct_factor %>% filter (t_gender == "Female Target")
rage_omni_sct_male_factor<-rage_omni_sct_factor %>% filter (t_gender == "Male Target")

            #### testing for target race x age interaction for female targets ####
mod_racexage_femtarget<-lmer(resp~t_race*age_c + (1|SID),data=rage_omni_sct_femtarget)
summary(mod_racexage_femtarget)

mod_racexage_femtarget.1<-lmer(resp~t_race*age_c + (1|SID),data=rage_omni_sct_fem_factor)
summary(mod_racexage_femtarget.1)

mod_racexage_maletarget<-lmer(resp~t_race*age_c + (1|SID),data=rage_omni_sct_maletarget)
summary(mod_racexage_maletarget)

mod_racexage_maletarget.1<-lmer(resp~t_race*age_c + (1|SID),data=rage_omni_sct_male_factor)
summary(mod_racexage_maletarget.1)


      ##### breaking down age interaction by target race #######
rage_omni_sct_blacktarget<-rage_omni_sct %>% filter (t_race == -1)
rage_omni_sct_whitetarget<-rage_omni_sct %>% filter (t_race == 0)
rage_omni_sct_asiantarget<-rage_omni_sct %>% filter (t_race == 1)

#testing for difference in slopes by gender for BLACK targets
mod_black_genxage<-lmer(resp~t_gender*age_c + (1|SID),
                        data=rage_omni_sct_blacktarget)
summary(mod_black_genxage)

#testing for difference in slopes by gender for WHITE targets
mod_white_genxage<-lmer(resp~t_gender*age_c + (1|SID),
                        data=rage_omni_sct_whitetarget)
summary(mod_white_genxage)

#testing for difference in slopes by gender for ASIAN targets
mod_asian_genxage<-lmer(resp~t_gender*age_c + (1|SID),
                        data=rage_omni_sct_asiantarget)
summary(mod_asian_genxage)



########## analyses for error blocks ###########

rage_omni_error <- rage_omni.1 %>% gather(stim,resp,16:21)
rage_omni_error<- rage_omni_error %>% separate(stim, into = c("fluff","stim","fluff2"),sep="_")
rage_omni_error<- rage_omni_error %>% separate(stim, into = c("t_race","t_gender"),sep=1)
rage_omni_error$t_race<-revalue(rage_omni_error$t_race,c("A"="Asian","B"="Black","W"="White"))
rage_omni_error$t_gender<-revalue(rage_omni_error$t_gender,c("F"="Female Target","M"="Male Target"))
rage_omni_error$fluff<-NULL
rage_omni_error$fluff2<-NULL

rage_omni_error$age_c<-scale(rage_omni_error$age,center=TRUE,scale=FALSE)
rage_omni_error$age_c<-as.numeric(rage_omni_error$age_c)

#### creating duplicate df for plotting later 
rage_omni_error_factor<-rage_omni_error
rage_omni_error_factor$t_race<-as.factor(rage_omni_error_factor$t_race)
rage_omni_error_factor$t_gender<-as.factor(rage_omni_error_factor$t_gender)

## recoding for contrast codes

rage_omni_error$t_gender[rage_omni_error$t_gender == "Male Target"] <- -0.5
rage_omni_error$t_gender[rage_omni_error$t_gender == "Female Target"] <- 0.5

rage_omni_error$t_race[rage_omni_error$t_race == "Black"] <- -1
rage_omni_error$t_race[rage_omni_error$t_race == "White"] <- 0
rage_omni_error$t_race[rage_omni_error$t_race == "Asian"] <- 1

rage_omni_error$t_gender<-as.numeric(rage_omni_error$t_gender)
rage_omni_error$t_race<-as.numeric(rage_omni_error$t_race)


# testing model for error trials
mod_omni_error<-glmer.nb(resp~t_race*t_gender + 
                           (1|SID),data=rage_omni_error,
                         control=glmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=100000)))
summary(mod_omni_error)

#pulling means for errors
mean_omni_error<-summarySEwithin(rage_omni_error_factor, measurevar="resp", 
  withinvars=c("t_race","t_gender"), 
  betweenvars = NULL,
  idvar="SID", na.rm=TRUE, conf.interval=.95)
mean_omni_error

#testing for age-related changes
mod_omni_error_age<-glmer.nb(resp~t_race*t_gender*age_c + 
                           (1|SID),data=rage_omni_error,
                         control=glmerControl(optimizer="bobyqa", 
                                              optCtrl=list(maxfun=100000)))
summary(mod_omni_error_age)

######## omnibus analyses for stereotype data ############

rage_omni_stereo<-rage_omni.1[c(1,5:7,46:70)]
rage_omni_stereo<-rage_omni_stereo %>% filter(stereotypedata==1)
rage_omni_stereo.1<-rage_omni_stereo %>% gather(stim,resp,c(6:29))
rage_omni_stereo.1<- rage_omni_stereo.1 %>% separate(stim, into = c("valence","trait","target"),sep="_")
rage_omni_stereo.1<- rage_omni_stereo.1 %>% separate(target, into = c("t_race","t_gender"),sep=1)

## saving factor levels for graphing
rage_omni_stereo.1$t_race<-revalue(rage_omni_stereo.1$t_race,c("b"="Black","w"="White","a"="Asian"))
rage_omni_stereo.1$t_gender<-revalue(rage_omni_stereo.1$t_gender,c("m"="Man","f"="Woman"))
rage_omni_stereo.1$valence<-revalue(rage_omni_stereo.1$valence,c("pos"="Positive","neg"="Negative"))
rage_omni_stereo_factor<-rage_omni_stereo.1
rage_omni_stereo_factor$t_race<-as.factor(rage_omni_stereo_factor$t_race)
rage_omni_stereo_factor$t_gender<-as.factor(rage_omni_stereo_factor$t_gender)
rage_omni_stereo_factor$valence<-as.factor(rage_omni_stereo_factor$valence)

#### recoding for contrast codes

rage_omni_stereo.1$t_gender[rage_omni_stereo.1$t_gender == "Man"] <- -0.5
rage_omni_stereo.1$t_gender[rage_omni_stereo.1$t_gender == "Woman"] <- 0.5
rage_omni_stereo.1$t_race[rage_omni_stereo.1$t_race == "Black"] <- -1
rage_omni_stereo.1$t_race[rage_omni_stereo.1$t_race == "White"] <- 0
rage_omni_stereo.1$t_race[rage_omni_stereo.1$t_race == "Asian"] <- 1
rage_omni_stereo.1$valence[rage_omni_stereo.1$valence == "Negative"] <- -0.5
rage_omni_stereo.1$valence[rage_omni_stereo.1$valence == "Positive"] <- 0.5
rage_omni_stereo.1$t_gender<-as.numeric(rage_omni_stereo.1$t_gender)
rage_omni_stereo.1$t_race<-as.numeric(rage_omni_stereo.1$t_race)
rage_omni_stereo.1$valence<-as.numeric(rage_omni_stereo.1$valence)

rage_omni_stereo.1$Kid_Eth<-as.character(rage_omni_stereo.1$Kid_Eth)
rage_omni_stereo.1$Kid_Eth[rage_omni_stereo.1$Kid_Eth == "White"] <- -0.5
rage_omni_stereo.1$Kid_Eth[rage_omni_stereo.1$Kid_Eth == "mixed"] <- 0.5
rage_omni_stereo.1$Kid_Eth[rage_omni_stereo.1$Kid_Eth == "Black"] <- 0.5
rage_omni_stereo.1$Kid_Eth[rage_omni_stereo.1$Kid_Eth == "Asian"] <- 0.5
rage_omni_stereo.1$Kid_Eth<-as.numeric(rage_omni_stereo.1$Kid_Eth)

rage_omni_stereo.1$age_c<-scale(rage_omni_stereo.1$age_exact, center = TRUE, scale = FALSE)
rage_omni_stereo.1$age_c<-as.numeric(rage_omni_stereo.1$age_c)
            

########### masculine stereotypes ###############
rage_omni_stereo_masc <- rage_omni_stereo.1 %>% 
  filter(trait == "masc" & t_gender == -0.5)
rage_omni_stereo_masc_factor <- rage_omni_stereo_factor %>% 
  filter(trait == "masc" & t_gender == "Man")

                    ##running basic model looking at choice as function of target race and valence
omni_mod_stereo_masc<-lmer(resp~t_race*valence + (1|SID),data=rage_omni_stereo_masc)
summary(omni_mod_stereo_masc) #main effect of target race
omni_mod_stereo_masc.1<-lmer(resp~t_race*valence + (1|SID),
                           data=rage_omni_stereo_masc_factor)
summary(omni_mod_stereo_masc.1)

## pulling means 
omni_emm_mascstereo<-emmeans(omni_mod_stereo_masc.1, "t_race",by = "valence")
omni_emm_mascstereo
pairs(omni_emm_mascstereo)

##looking at interaction with age
omni_mod_stereo_masc_age<-lmer(resp~t_race*valence*age_c + (1|SID),data=rage_omni_stereo_masc)
summary(omni_mod_stereo_masc_age) # no effect

## looking at interaction with kid racial majority/minority status
omni_mod_stereo_masc_kideth<-lmer(resp~t_race*valence*Kid_Eth + (1|SID),data=rage_omni_stereo_masc)
summary(omni_mod_stereo_masc_kideth) # no effect

                  ######### feminine stereotypes ##########
rage_omni_stereo_fem <- rage_omni_stereo.1 %>% 
  filter(trait == "fem" & t_gender == 0.5)
rage_omni_stereo_fem_factor <- rage_omni_stereo_factor %>% 
  filter(trait == "fem" & t_gender == "Woman")

#running the model
omni_mod_stereo_fem<-lmer(resp~t_race*valence + (1|SID),data=rage_omni_stereo_fem)
summary(omni_mod_stereo_fem)

#rage_omni_stereo_fem_factor<-within(rage_omni_stereo_fem_factor,
 #                               t_race<-relevel(t_race, ref="White"))
omni_mod_stereo_fem.1<-lmer(resp~t_race*valence + (1|SID),
                          data=rage_omni_stereo_fem_factor)
summary(omni_mod_stereo_fem.1)

omni_emm_femstereo<-emmeans(omni_mod_stereo_fem.1, "t_race",by = "valence")
omni_emm_femstereo
pairs(omni_emm_femstereo)

### looking at the interaction with age
omni_mod_stereo_fem_age<-lmer(resp~t_race*valence*age_c + (1|SID),data=rage_omni_stereo_fem)
summary(omni_mod_stereo_fem_age) # no effects (well, marginal intx between target race and age)

#looking at interaction by biracial status
omni_mod_stereo_fem_kideth<-lmer(resp~t_race*valence*Kid_Eth + (1|SID),data=rage_omni_stereo_fem)
summary(omni_mod_stereo_fem_kideth) #no effect




######### testing for moderation of SCT results by environmental factors ##########

rage_omni_sct$blackfriends_c<-scale(rage_omni_sct$Friends_Black, center=TRUE,scale=FALSE)
rage_omni_sct$Friends_White<-as.numeric(rage_omni_sct$Friends_White)
rage_omni_sct$sum_friends<-rage_omni_sct$Friends_Asian+rage_omni_sct$Friends_White+rage_omni_sct$Friends_Black
rage_omni_sct$perc_blackfriends<-rage_omni_sct$Friends_Black/rage_omni_sct$sum_friends

#are categorization speeds moderated by number of black friends
mod_sct_blackfriends<-lmer(resp~t_race*t_gender*blackfriends_c + (1|SID),
                           data=rage_omni_sct)
summary(mod_sct_blackfriends) #NO - by raw numbers of Black friends

mod_sct_perc_blackfriends<-lmer(resp~t_race*t_gender*perc_blackfriends + (1|SID),
                           data=rage_omni_sct)
summary(mod_sct_perc_blackfriends) #not by percentage of black friends

#what about neighborhood diversity?
rage_omni_sct$zip_Black_c<-scale(rage_omni_sct$Zip_Perc_Black, center=TRUE,scale=FALSE)

mod_sct_zipBlack<-lmer(resp~t_race*t_gender*zip_Black_c + (1|SID),
                           data=rage_omni_sct)
summary(mod_sct_zipBlack) #NO - not by percentage of black people


######### testing for moderation of SCT results by parental factors ##########
#what about subjective SES?
rage_omni_sct$subjSES_c<-scale(rage_omni_sct$SubjSES, center=TRUE,scale=FALSE)
mod_sct_subjSES<-lmer(resp~t_race*t_gender*subjSES_c + (1|SID),
                       data=rage_omni_sct)
summary(mod_sct_subjSES) #NO - not by subjSES

#what about parental colorblind ideology
rage_omni_sct$colorblind_c<-scale(rage_omni_sct$CB_Overall, center=TRUE,scale=FALSE)
mod_sct_parent_cb<-lmer(resp~t_race*t_gender*colorblind_c + (1|SID),
                      data=rage_omni_sct)
summary(mod_sct_parent_cb) #NO - not by colorblind ideology

#what about multicultural ideology?
rage_omni_sct$multic_c<-scale(rage_omni_sct$MC_Overall, center=TRUE,scale=FALSE)
mod_sct_parent_mc<-lmer(resp~t_race*t_gender*multic_c + (1|SID),
                        data=rage_omni_sct)
summary(mod_sct_parent_mc) #NO - not by multicultural ideology

#what about political ideology
#testing liberal ideology first
rage_omni_sct$lib_c<-scale(rage_omni_sct$LibIdeo, center=TRUE,scale=FALSE)
mod_sct_parent_lib<-lmer(resp~t_race*t_gender*lib_c + (1|SID),
                        data=rage_omni_sct)
summary(mod_sct_parent_lib) #NO - not by liberal ideology
#what about conservative ideology
rage_omni_sct$cons_c<-scale(rage_omni_sct$ConsIdeo, center=TRUE,scale=FALSE)
mod_sct_parent_cons<-lmer(resp~t_race*t_gender*cons_c + (1|SID),
                         data=rage_omni_sct)
summary(mod_sct_parent_cons) #NO - not by conservative ideology


########### examining correlations between measures
rage_omni.1$Friends_White<-as.numeric(rage_omni.1$Friends_White)
rage_enviro_corr_rt<-rage_omni.1[c(1,5,10:15,22:32,47:70)]
rage_enviro_corr_mistakes<-rage_omni.1[c(1,5,16:32,47:70)]

### examining correlations for RT DV
rage_enviro_corr_rt$wb_friend_diff<-rage_enviro_corr_rt$Friends_White-rage_enviro_corr_rt$Friends_Black
rage_enviro_corr_rt$wb_zip_diff<-rage_enviro_corr_rt$Zip_Perc_White-rage_enviro_corr_rt$Zip_Perc_Black
rage_enviro_corr_rt$mc_cb_diff<-rage_enviro_corr_rt$MC_Overall-rage_enviro_corr_rt$CB_Overall

rage_enviro_corr_rt$friend_total<-rage_enviro_corr_rt$Friends_Asian+rage_enviro_corr_rt$Friends_Black+rage_enviro_corr_rt$Friends_White
rage_enviro_corr_rt$perc_black_friend<-rage_enviro_corr_rt$Friends_Black/rage_enviro_corr_rt$friend_total
rage_enviro_corr_rt$perc_white_friend<-rage_enviro_corr_rt$Friends_White/rage_enviro_corr_rt$friend_total
rage_enviro_corr_rt$perc_asian_friend<-rage_enviro_corr_rt$Friends_Asian/rage_enviro_corr_rt$friend_total

rage_enviro_corr_rt.1<-rage_enviro_corr_rt[c(3:8,45,48:50)]

rage_corr.1<-corr.test(rage_enviro_corr_rt.1)
rage_corr.1

####### graphing correlations
rage_omni_sct_factor$Friends_White<-as.numeric(rage_omni_sct_factor$Friends_White)
rage_omni_sct_factor$friend_total<-rage_omni_sct_factor$Friends_Asian+rage_omni_sct_factor$Friends_Black+rage_omni_sct_factor$Friends_White
rage_omni_sct_factor$perc_black_friend<-rage_omni_sct_factor$Friends_Black/rage_omni_sct_factor$friend_total
rage_omni_sct_factor$perc_white_friend<-rage_omni_sct_factor$Friends_White/rage_omni_sct_factor$friend_total
rage_omni_sct_factor$perc_asian_friend<-rage_omni_sct_factor$Friends_Asian/rage_omni_sct_factor$friend_total


corr_rt_perc_blackfriends_plot<-ggplot(rage_omni_sct_factor,
                     aes(x=perc_black_friend, y=resp))+
  geom_point(stat="identity", size = 1.5, alpha = .4, colour = "#014d64")+
  geom_smooth(method=lm, colour = "#01a2d9")+
  facet_wrap(~t_race*t_gender)+
  scale_y_continuous(limits = c(500, 1500))+
  labs(y = "Categorization Speed",x= "% of Friends That Are Black") 
  
corr_rt_perc_blackfriends_plot

corr_rt_perc_whitefriends_plot<-ggplot(rage_omni_sct_factor,
                                       aes(x=perc_white_friend, y=resp))+
  geom_point(stat="identity", size = 1.5, alpha = .4, colour = "#014d64")+
  geom_smooth(method=lm, colour = "#01a2d9")+
  facet_wrap(~t_race*t_gender)+
  scale_y_continuous(limits = c(500, 1500))+
  labs(y = "Categorization Speed",x= "% of Friends That Are White") 

corr_rt_perc_whitefriends_plot

friends_corr_combined<-ggarrange(corr_rt_perc_blackfriends_plot,corr_rt_perc_whitefriends_plot)
friends_corr_combined
#examining correlations for the mistakes DV
rage_enviro_corr_mistakes$wb_friend_diff<-rage_enviro_corr_mistakes$Friends_White-rage_enviro_corr_mistakes$Friends_Black
rage_enviro_corr_mistakes$wb_zip_diff<-rage_enviro_corr_mistakes$Zip_Perc_White-rage_enviro_corr_mistakes$Zip_Perc_Black
rage_enviro_corr_mistakes$mc_cb_diff<-rage_enviro_corr_mistakes$MC_Overall-rage_enviro_corr_mistakes$CB_Overall

rage_enviro_corr_mistakes$friend_total<-rage_enviro_corr_mistakes$Friends_Asian+rage_enviro_corr_mistakes$Friends_Black+rage_enviro_corr_mistakes$Friends_White
rage_enviro_corr_mistakes$perc_black_friend<-rage_enviro_corr_mistakes$Friends_Black/rage_enviro_corr_mistakes$friend_total
rage_enviro_corr_mistakes$perc_white_friend<-rage_enviro_corr_mistakes$Friends_White/rage_enviro_corr_mistakes$friend_total
rage_enviro_corr_mistakes$perc_asian_friend<-rage_enviro_corr_mistakes$Friends_Asian/rage_enviro_corr_mistakes$friend_total

rage_enviro_corr_mistakes.1<-rage_enviro_corr_mistakes[c(3:8,45,48:50)]

#printing the correlation matrix for mistake DV
rage_corr.2<-corr.test(rage_enviro_corr_mistakes.1)
rage_corr.2
