#RAGE Study 2 R file

lib<-c("lme4","readr","tidyr","effects","ggplot2","psych","MASS",
       "Rmisc","plyr","dplyr","lmerTest","ggthemes","emmeans","pastecs")
lapply(lib,require,character.only=TRUE)

rage_omni <- read.csv(file.choose()) #read in the omnibus data file
rage_omni.1<-rage_omni
rage_omni.1$Study[which(rage_omni.1$Study==1)] <- -1 #monoracial study
rage_omni.1$Study[which(rage_omni.1$Study==2)] <- 1 #biracial vs. White comparison Study

rage_2<-rage_omni.1 %>% filter(Study == 1)

###### analyzing validation trials #########
rage2_val <- rage_2 %>% gather(stim,resp,8:9)
colnames(rage2_val)[colnames(rage2_val)=="stim"] <- "Stimulus"

#main analyses
mod_val<-lmer(resp~Stimulus + (1|SID),data=rage2_val)
summary(mod_val)

emm_val<-emmeans(mod_val, "Stimulus")
emm_val

########## analysis for critical speed blocks #############
rage2_sct <- rage_2 %>% gather(stim,resp,10:15)
rage2_sct<- rage2_sct %>% separate(stim, into = c("fluff","stim","fluff2"),sep="_")
rage2_sct$fluff<-NULL
rage2_sct$fluff2<-NULL
rage2_sct<- rage2_sct %>% separate(stim, into = c("t_race","t_gender"),sep=1)
rage2_sct$t_race<-revalue(rage2_sct$t_race,c("A"="Asian","B"="Black","W"="White"))
rage2_sct$t_gender<-revalue(rage2_sct$t_gender,c("F"="Female Target","M"="Male Target"))

#### creating duplicate df for plotting later 
rage2_sct_factor<-rage2_sct
rage2_sct_factor$t_race<-as.factor(rage2_sct_factor$t_race)
rage2_sct_factor$t_gender<-as.factor(rage2_sct_factor$t_gender)

#### recoding factors as numeric contrasts
rage2_sct$t_gender[rage2_sct$t_gender == "Male Target"] <- -0.5
rage2_sct$t_gender[rage2_sct$t_gender == "Female Target"] <- 0.5

rage2_sct$t_race[rage2_sct$t_race == "Black"] <- -1
rage2_sct$t_race[rage2_sct$t_race == "White"] <- 0
rage2_sct$t_race[rage2_sct$t_race == "Asian"] <- 1

rage2_sct$t_gender<-as.numeric(rage2_sct$t_gender)
rage2_sct$t_race<-as.numeric(rage2_sct$t_race)

#### testing for main race x gender interaction
mod_sct<-lmer(resp~t_race*t_gender + (1|SID),data=rage2_sct)
summary(mod_sct)

#testing for specific race comparisons
mod_sct.1<-lmer(resp~t_race*t_gender + (1|SID),data=rage2_sct_factor)
summary(mod_sct.1)

r2_emm_gen_sct<-emmeans(mod_sct.1, "t_gender", by="t_race") #gender comparison by race
pairs(r2_emm_gen_sct)
r2_emm_race_sct<-emmeans(mod_sct.1, "t_race", by="t_gender") #race comparison by gender
r2_emm_race_sct
pairs(r2_emm_race_sct)
### calling main race x gender table
r2_mean_sct <- summarySEwithin(rage2_sct_factor, measurevar="resp", 
                            withinvars=c("t_race","t_gender"), 
                            betweenvars = NULL,
                            idvar="SID", na.rm=TRUE, conf.interval=.95)
r2_mean_sct<-within(r2_mean_sct, t_race<-relevel(t_race, ref = "White"))
r2_mean_sct<-within(r2_mean_sct, t_race<-relevel(t_race, ref = "Asian"))
r2_mean_sct<-within(r2_mean_sct, t_gender<-relevel(t_gender, ref = "Male Target"))
r2_mean_sct

rage2_sct_factor<-within(rage2_sct_factor, t_race<-relevel(t_race, ref = "White"))
rage2_sct_factor<-within(rage2_sct_factor, t_race<-relevel(t_race, ref = "Asian"))
rage2_sct_factor<-within(rage2_sct_factor, t_gender<-relevel(t_gender, ref = "Male Target"))

######## graphing main interaction ############

r2_plot_sct_dv<-ggplot() +
  geom_jitter(data=rage2_sct_factor, aes(x=t_gender, y=resp,colour = factor(t_gender),shape=t_gender),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
  geom_point(data = r2_mean_sct, 
             aes(x = t_gender, y = resp, colour = t_gender, shape=t_gender), 
             stat = "identity", size = 4) +
  geom_errorbar(data = r2_mean_sct, width = .1, 
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

r2_plot_sct_dv  

####### looking at interaction by age #########
#centering age variable
rage2_sct$age_c<-scale(rage2_sct$age,center=TRUE,scale=FALSE)
rage2_sct$age_c<-as.numeric(rage2_sct$age_c)

rage2_sct_factor$age_c<-scale(rage2_sct_factor$age,
                                  center=TRUE,scale=FALSE)
rage2_sct_factor$age_c<-as.numeric(rage2_sct_factor$age_c)

# running model for testing age interaction
r2_mod_sct_age<-lmer(resp~t_race*t_gender*age_c + (1|SID),data=rage2_sct)
summary(r2_mod_sct_age) #three-way interaction by age

          ######## graphing age interaction #########

df_age<-effect(term="t_race:t_gender*age_c",
               xlevels=list(t_race=c(-1,0,1),
                            t_gender=c(-.5,.5),
                            age_c=c(-2,-1,0,1,2)), 
               mod=r2_mod_sct_age)
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
rage2_sct_factor<-within(rage2_sct_factor, 
                             t_gender<-relevel(t_gender, ref = "Male Target"))

### graph code
r2_plot_age<-ggplot(data = df_age, 
                 aes(x = age_c, y = fit, linetype= t_gender))+
  geom_jitter(data=rage2_sct_factor, 
              aes(x=age_exact, y=resp, colour = t_gender,shape=t_gender),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
  geom_line(aes(group=t_gender))+ 
  #geom_smooth(method="glm",fullrange=TRUE)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=t_gender),alpha=0.3) +
  scale_fill_manual(values=c("#01a2d9","#014d64"))+
  scale_color_manual(values=c("#01a2d9","#014d64"))+
  scale_y_continuous(limits = c(900, 1600))+
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

r2_plot_age

          ##### breaking down age interaction by target gender #######
rage2_sct_femtarget<-rage2_sct %>% filter (t_gender == 0.5)
rage2_sct_maletarget<-rage2_sct %>% filter (t_gender == -0.5)
rage2_sct_fem_factor<-rage2_sct_factor %>% filter (t_gender == "Female Target")
rage2_sct_male_factor<-rage2_sct_factor %>% filter (t_gender == "Male Target")

          #### testing for target race x age interaction for female targets ####
mod_racexage_femtarget<-lmer(resp~t_race*age_c + (1|SID),data=rage2_sct_femtarget)
summary(mod_racexage_femtarget)

mod_racexage_femtarget.1<-lmer(resp~t_race*age_c + (1|SID),data=rage2_sct_fem_factor)
summary(mod_racexage_femtarget.1)
          
          #### testing for target race x age interaction for MALE targets ####
mod_racexage_maletarget<-lmer(resp~t_race*age_c + (1|SID),data=rage2_sct_maletarget)
summary(mod_racexage_maletarget)

mod_racexage_maletarget.1<-lmer(resp~t_race*age_c + (1|SID),data=rage2_sct_male_factor)
summary(mod_racexage_maletarget.1)

          ##### breaking down age interaction by target race #######
rage2_sct_blacktarget<-rage2_sct %>% filter (t_race == -1)
rage2_sct_whitetarget<-rage2_sct %>% filter (t_race == 0)
rage2_sct_asiantarget<-rage2_sct %>% filter (t_race == 1)

#testing for difference in slopes by gender for BLACK targets
r2_mod_black_genxage<-lmer(resp~t_gender*age_c + (1|SID),
                        data=rage2_sct_blacktarget)
summary(r2_mod_black_genxage)

#testing for difference in slopes by gender for WHITE targets
r2_mod_white_genxage<-lmer(resp~t_gender*age_c + (1|SID),
                        data=rage2_sct_whitetarget)
summary(r2_mod_white_genxage)

#testing for difference in slopes by gender for ASIAN targets
r2_mod_asian_genxage<-lmer(resp~t_gender*age_c + (1|SID),
                        data=rage2_sct_asiantarget)
summary(r2_mod_asian_genxage)


########### examining  interaction by monoracial vs. biracial status #########
rage2_sct$Kid_Eth<-as.character(rage2_sct$Kid_Eth)
rage2_sct$Kid_Eth[rage2_sct$Kid_Eth == "mixed"] <- 0.5
rage2_sct$Kid_Eth[rage2_sct$Kid_Eth == "White"] <- -0.5
rage2_sct$Kid_Eth<-as.numeric(rage2_sct$Kid_Eth)

#testing model for kid racial status
r2_mod_sct_kideth<-lmer(resp~t_race*t_gender*Kid_Eth + (1|SID),data=rage2_sct)
summary(r2_mod_sct_kideth)

r2_mod_sct_age_kideth<-lmer(resp~t_race*t_gender*age_c*Kid_Eth + (1|SID),data=rage2_sct)
summary(r2_mod_sct_age_kideth)

########## analyses for error blocks ###########

rage2_error <- rage_2 %>% gather(stim,resp,16:21)
rage2_error<- rage2_error %>% separate(stim, into = c("fluff","stim","fluff2"),sep="_")
rage2_error<- rage2_error %>% separate(stim, into = c("t_race","t_gender"),sep=1)
rage2_error$t_race<-revalue(rage2_error$t_race,c("A"="Asian","B"="Black","W"="White"))
rage2_error$t_gender<-revalue(rage2_error$t_gender,c("F"="Female Target","M"="Male Target"))
rage2_error$fluff<-NULL
rage2_error$fluff2<-NULL

rage2_error$age_c<-scale(rage2_error$age,center=TRUE,scale=FALSE)
rage2_error$age_c<-as.numeric(rage2_error$age_c)

#### creating duplicate df for plotting later 
rage2_error_factor<-rage2_error
rage2_error_factor$t_race<-as.factor(rage2_error_factor$t_race)
rage2_error_factor$t_gender<-as.factor(rage2_error_factor$t_gender)

## recoding for contrast codes

rage2_error$t_gender[rage2_error$t_gender == "Male Target"] <- -0.5
rage2_error$t_gender[rage2_error$t_gender == "Female Target"] <- 0.5

rage2_error$t_race[rage2_error$t_race == "Black"] <- -1
rage2_error$t_race[rage2_error$t_race == "White"] <- 0
rage2_error$t_race[rage2_error$t_race == "Asian"] <- 1

rage2_error$t_gender<-as.numeric(rage2_error$t_gender)
rage2_error$t_race<-as.numeric(rage2_error$t_race)

# testing model for error trials
mod2_error<-glmer.nb(resp~t_race*t_gender + 
                           (1|SID),data=rage2_error,
                         control=glmerControl(optimizer="bobyqa", 
                                              optCtrl=list(maxfun=100000)))
summary(mod2_error)

#pulling means for errors
mean2_error<-summarySEwithin(rage2_error_factor, measurevar="resp", 
                                 withinvars=c("t_race","t_gender"), 
                                 betweenvars = NULL,
                                 idvar="SID", na.rm=TRUE, conf.interval=.95)
mean2_error

#testing for age-related changes
mod2_error_age<-glmer.nb(resp~t_race*t_gender*age_c + 
                               (1|SID),data=rage2_error,
                             control=glmerControl(optimizer="bobyqa", 
                                                  optCtrl=list(maxfun=100000)))
summary(mod2_error_age) # no age-related changes in errors

### testing for effects of biracial status

rage2_error$Kid_Eth<-as.character(rage2_error$Kid_Eth)
rage2_error$Kid_Eth[rage2_error$Kid_Eth == "mixed"] <- 0.5
rage2_error$Kid_Eth[rage2_error$Kid_Eth == "White"] <- -0.5
rage2_error$Kid_Eth<-as.numeric(rage2_error$Kid_Eth)

#running model
mod2_error_kideth<-glmer.nb(resp~t_race*t_gender*Kid_Eth + 
                           (1|SID),data=rage2_error,
                         control=glmerControl(optimizer="bobyqa", 
                                              optCtrl=list(maxfun=100000)))
summary(mod2_error_kideth) # no three-way interaction

#testing for 4-way interaction
mod2_error_age_kideth<-glmer.nb(resp~t_race*t_gender*age_c*Kid_Eth + 
                              (1|SID),data=rage2_error,
                            control=glmerControl(optimizer="bobyqa", 
                                                 optCtrl=list(maxfun=100000)))
summary(mod2_error_age_kideth)

######### analyzing stereotype data in study 2 #########

rage2_stereo<-rage_2[c(1,5:7,46:70)]
rage2_stereo<-rage2_stereo %>% filter(stereotypedata==1)
rage2_stereo.1<-rage2_stereo %>% gather(stim,resp,c(6:29))
rage2_stereo.1<- rage2_stereo.1 %>% separate(stim, into = c("valence","trait","target"),sep="_")
rage2_stereo.1<- rage2_stereo.1 %>% separate(target, into = c("t_race","t_gender"),sep=1)

## saving factor levels for graphing
rage2_stereo.1$t_race<-revalue(rage2_stereo.1$t_race,c("b"="Black","w"="White","a"="Asian"))
rage2_stereo.1$t_gender<-revalue(rage2_stereo.1$t_gender,c("m"="Man","f"="Woman"))
rage2_stereo.1$valence<-revalue(rage2_stereo.1$valence,c("pos"="Positive","neg"="Negative"))
rage2_stereo_factor<-rage2_stereo.1
rage2_stereo_factor$t_race<-as.factor(rage2_stereo_factor$t_race)
rage2_stereo_factor$t_gender<-as.factor(rage2_stereo_factor$t_gender)
rage2_stereo_factor$valence<-as.factor(rage2_stereo_factor$valence)

#### recoding for contrast codes

rage2_stereo.1$t_gender[rage2_stereo.1$t_gender == "Man"] <- -0.5
rage2_stereo.1$t_gender[rage2_stereo.1$t_gender == "Woman"] <- 0.5
rage2_stereo.1$t_race[rage2_stereo.1$t_race == "Black"] <- -1
rage2_stereo.1$t_race[rage2_stereo.1$t_race == "White"] <- 0
rage2_stereo.1$t_race[rage2_stereo.1$t_race == "Asian"] <- 1
rage2_stereo.1$valence[rage2_stereo.1$valence == "Negative"] <- -0.5
rage2_stereo.1$valence[rage2_stereo.1$valence == "Positive"] <- 0.5
rage2_stereo.1$t_gender<-as.numeric(rage2_stereo.1$t_gender)
rage2_stereo.1$t_race<-as.numeric(rage2_stereo.1$t_race)
rage2_stereo.1$valence<-as.numeric(rage2_stereo.1$valence)

rage2_stereo.1$Kid_Eth<-as.character(rage2_stereo.1$Kid_Eth)
rage2_stereo.1$Kid_Eth[rage2_stereo.1$Kid_Eth == "White"] <- -0.5
rage2_stereo.1$Kid_Eth[rage2_stereo.1$Kid_Eth == "mixed"] <- 0.5
rage2_stereo.1$Kid_Eth<-as.numeric(rage2_stereo.1$Kid_Eth)

rage2_stereo.1$age_c<-scale(rage2_stereo.1$age_exact, center = TRUE, scale = FALSE)
rage2_stereo.1$age_c<-as.numeric(rage2_stereo.1$age_c)
########### masculine stereotypes ###############
rage2_stereo_masc <- rage2_stereo.1 %>% 
  filter(trait == "masc" & t_gender == -0.5)
rage2_stereo_masc_factor <- rage2_stereo_factor %>% 
  filter(trait == "masc" & t_gender == "Man")

##running basic model looking at choice as function of target race and valence
r2_mod_stereo_masc<-lmer(resp~t_race*valence + (1|SID),data=rage2_stereo_masc)
summary(r2_mod_stereo_masc) #main effect of target race
r2_mod_stereo_masc.1<-lmer(resp~t_race + (1|SID),
                           data=rage2_stereo_masc_factor)
summary(r2_mod_stereo_masc.1)

##looking at interaction with age
r2_mod_stereo_masc_age<-lmer(resp~t_race*valence*age_c + (1|SID),data=rage2_stereo_masc)
summary(r2_mod_stereo_masc_age) # no effect

## looking at interaction with kid biracial status
r2_mod_stereo_masc_kideth<-lmer(resp~t_race*valence*Kid_Eth + (1|SID),data=rage2_stereo_masc)
summary(r2_mod_stereo_masc_kideth) # no effect

r2_emm_mascstereo<-emmeans(r2_mod_stereo_masc.1, "t_race")
r2_emm_mascstereo


######### feminine stereotypes ##########
rage2_stereo_fem <- rage2_stereo.1 %>% 
  filter(trait == "fem" & t_gender == 0.5)
rage2_stereo_fem_factor <- rage2_stereo_factor %>% 
  filter(trait == "fem" & t_gender == "Woman")

#running the model
r2_mod_stereo_fem<-lmer(resp~t_race*valence + (1|SID),data=rage2_stereo_fem)
summary(r2_mod_stereo_fem)

rage2_stereo_fem_factor<-within(rage2_stereo_fem_factor,
                                t_race<-relevel(t_race, ref="White"))
r2_mod_stereo_fem.1<-lmer(resp~t_race + (1|SID),
                           data=rage2_stereo_fem_factor)
summary(r2_mod_stereo_fem.1)

r2_emm_femstereo<-emmeans(r2_mod_stereo_fem.1, "t_race")
r2_emm_femstereo

### looking at the interaction with age
r2_mod_stereo_fem_age<-lmer(resp~t_race*valence*age_c + (1|SID),data=rage2_stereo_fem)
summary(r2_mod_stereo_fem_age) # no effects (well, marginal intx between target race and age)

#looking at interaction by biracial status
r2_mod_stereo_fem_kideth<-lmer(resp~t_race*valence*Kid_Eth + (1|SID),data=rage2_stereo_fem)
summary(r2_mod_stereo_fem_kideth) #no effect

