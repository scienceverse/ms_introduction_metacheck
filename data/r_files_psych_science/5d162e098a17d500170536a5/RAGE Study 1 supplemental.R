## RAGE Study 1 supplemental analyses

lib<-c("lme4","readr","tidyr","effects","ggplot2","psych","MASS",
       "Rmisc","plyr","dplyr","lmerTest","ggthemes","lsmeans","pastecs","ggpubr")
lapply(lib,require,character.only=TRUE)

rage<-read.csv(file.choose()) #read in RAGE_Datavyu file
rage.1<-rage[c(1,4:82)]
View(rage.1)

########### TESTING OWN RACE BIAS ACCOUNT  ############

rage_new_orb<-rage.1 %>% gather(stim,resp,13:16)
rage_new_orb<- rage_new_orb %>% separate(stim, into = c("platform","own","t_gender"),sep="_")
rage_new_orb$platform<-NULL
rage_new_orb$own<-revalue(rage_new_orb$own,c("OwnRace"="Self","DiffRace"="Others"))
rage_new_orb$own<-as.factor(rage_new_orb$own)
rage_new_orb$t_gender<-as.factor(rage_new_orb$t_gender)
rage_new_orb$Kid_Eth<-as.character(rage_new_orb$Kid_Eth)
rage_new_orb$Kid_Eth[rage_new_orb$Kid_Eth == "Black"] <- -1
rage_new_orb$Kid_Eth[rage_new_orb$Kid_Eth == "White"] <- 0
rage_new_orb$Kid_Eth[rage_new_orb$Kid_Eth == "Asian"] <- 1
rage_new_orb$Kid_Eth<-as.numeric(rage_new_orb$Kid_Eth)


mod_dv_orb<-lmer(resp~own + (1|SID),data=rage_new_orb) #an effect of own-race bias? no
summary(mod_dv_orb)

mod_dv_orb_kideth<-lmer(resp~own*Kid_Eth + (1|SID),data=rage_new_orb) 
summary(mod_dv_orb_kideth) #interaction between own-race and kid eth, but shows that Black kids are slower to ID own race

mean_orb_kideth <- summarySEwithin(rage_new_orb, measurevar="resp", 
                                   withinvars="own", 
                                   betweenvars = "Kid_Eth",
                                   idvar="SID", na.rm=TRUE, conf.interval=.95)
mean_orb_kideth

mod_dv_smth<-lmer(resp~own*t_gender + (1|SID),data=rage_new_orb) #an effect of subordinate male target hypothesis? No
summary(mod_dv_smth)

mean_orb <- summarySEwithin(rage_new_orb, measurevar="resp", 
                            withinvars=c("own","t_gender"), 
                            betweenvars = NULL,
                            idvar="SID", na.rm=TRUE, conf.interval=.95)
mean_orb$own<-as.factor(mean_orb$own)
mean_orb$t_gender<-as.factor(mean_orb$t_gender)
mean_orb

###### considering subordinate male target hypothesis#########

rage_new_orb.1<-rage_new_orb

rage_new_orb.1$Kid_Gender<-as.character(rage_new_orb.1$Kid_Gender)
rage_new_orb.1$Kid_Gender[rage_new_orb.1$Kid_Gender == "Male"] <- -0.5
rage_new_orb.1$Kid_Gender[rage_new_orb.1$Kid_Gender == "Female"] <- 0.5
rage_new_orb.1$Kid_Gender<-as.numeric(rage_new_orb.1$Kid_Gender)

rage_new_orb.1$age_c<-scale(rage_new_orb.1$age_exact,center=TRUE,scale=FALSE)

mod_dv_smth<-lmer(resp~own*t_gender + (1|SID),data=rage_new_orb.1)
summary(mod_dv_smth) #3

mod_dv_orb_eth<-lmer(resp~own*t_gender*Kid_Eth + (1|SID),data=rage_new_orb.1)
summary(mod_dv_orb_eth) # marginal 3-way interaction

mod_dv_orb_age<-lmer(resp~own*t_gender*age_c + (1|SID),data=rage_new_orb.1)
summary(mod_dv_orb_age) #3-way interaction with age? no.


mean_orb_kideth_smth <- summarySEwithin(rage_new_orb, measurevar="resp", 
                                        withinvars=c("own","t_gender"), 
                                        betweenvars = "Kid_Eth",
                                        idvar="SID", na.rm=TRUE, conf.interval=.95)
#mean_orb_kideth$own<-revalue(mean_orb_kideth$own, c("own"="Own Race","diff"="Different Race"))
mean_orb_kideth_smth
mean_orb_kideth_smth<-within(mean_orb_kideth_smth, Kid_Eth<-relevel(Kid_Eth, ref = "Asian"))
rage_new_orb<-within(rage_new_orb, Kid_Eth<-relevel(Kid_Eth, ref = "Asian"))

plot_orb_kideth_smth<-ggplot() +
  geom_jitter(data=rage_new_orb, aes(x=Kid_Eth, y=resp,colour = factor(own),shape=own),
              stat="identity", size = 1.5, alpha = .4, height = 0, width = 0.25) +
  geom_point(data = mean_orb_kideth_smth, 
             aes(x = Kid_Eth, y = resp, colour = own,shape=own), 
             stat = "identity", size = 5) +
  geom_errorbar(data = mean_orb_kideth, width = .1, 
                aes(x = factor(Kid_Eth), ymin = resp-ci, ymax = resp+ci, colour = factor(own))) +
  ylim(800,1300) +
  scale_x_discrete(labels=c("Asian","White","Black"))+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_economist() +
  labs(y = "Categorization Speed", x = "Child's Race", colour ="Self or Other",shape="Self or Other") 
plot_orb_kideth_smth

mod_dv_orb_eth_pgen<-lmer(resp~own*Kid_Eth*Kid_Gender + (1|SID),data=rage_new_orb.1)
summary(mod_dv_orb_eth_pgen) #no moderation by p_gen -- black girls and boys show the same pattern

rage_new_orb.1_blackps<-rage_new_orb.1 %>% filter (Kid_Eth==-1)
mod_orb_blackps<-lmer(resp~own+ (1|SID),data=rage_new_orb.1_blackps)
summary(mod_orb_blackps)
mod_orb_blackps.1<-lmer(resp~own*age_exact+ (1|SID),data=rage_new_orb.1_blackps)
summary(mod_orb_blackps.1)

rage_new_orb.1_whiteps<-rage_new_orb.1 %>% filter (Kid_Eth==0)
mod_orb_whiteps<-lmer(resp~own+ (1|SID),data=rage_new_orb.1_whiteps)
summary(mod_orb_whiteps)
mod_orb_whiteps.1<-lmer(resp~own*age_exact+ (1|SID),data=rage_new_orb.1_whiteps)
summary(mod_orb_whiteps.1)

rage_new_orb.1_asianps<-rage_new_orb.1 %>% filter (Kid_Eth==1)
mod_orb_asianps<-lmer(resp~own+ (1|SID),data=rage_new_orb.1_asianps)
summary(mod_orb_asianps)
mod_orb_asianps.1<-lmer(resp~own*age_exact+ (1|SID),data=rage_new_orb.1_asianps)
summary(mod_orb_asianps.1)


rage_new_orb$age_c<-scale(rage_new_orb$age_exact,center=TRUE,scale=FALSE)
mod_dv_orb_ethxage<-lmer(resp~own*Kid_Eth*age_c + (1|SID),data=rage_new_orb)
summary(mod_dv_orb_ethxage) # no higher order moderation with age -- main effect of age 

####### TESTING LIKE-ME HYPOTHESIS  #############
rage_dv_likeme<-rage.1 %>% gather(stim,resp,17:18)
rage_dv_likeme$stim<-revalue(rage_dv_likeme$stim,c("DV_LikeMe"="Self","DV_NotLikeMe"="Others"))

rage_dv_likeme$Kid_Eth<-as.character(rage_dv_likeme$Kid_Eth)
rage_dv_likeme$Kid_Eth[rage_dv_likeme$Kid_Eth == "Black"] <- -1
rage_dv_likeme$Kid_Eth[rage_dv_likeme$Kid_Eth == "White"] <- 0
rage_dv_likeme$Kid_Eth[rage_dv_likeme$Kid_Eth == "Asian"] <- 1
rage_dv_likeme$Kid_Eth<-as.numeric(rage_dv_likeme$Kid_Eth)

rage_dv_likeme$Kid_Gender<-as.character(rage_dv_likeme$Kid_Gender)
rage_dv_likeme$Kid_Gender[rage_dv_likeme$Kid_Gender == "Male"] <- -0.5
rage_dv_likeme$Kid_Gender[rage_dv_likeme$Kid_Gender == "Female"] <- 0.5
rage_dv_likeme$Kid_Gender<-as.numeric(rage_dv_likeme$Kid_Gender)

mod_likeme_dv<-lmer(resp~stim + (1|SID),data=rage_dv_likeme)
summary(mod_likeme_dv) #no main effect of like me

mod_likeme_dv.1<-lmer(resp~stim*age_exact + (1|SID),data=rage_dv_likeme)
summary(mod_likeme_dv.1) #no like me x age interaction

mod_likeme_dv_eth<-lmer(resp~stim*Kid_Eth + (1|SID),data=rage_dv_likeme)
summary(mod_likeme_dv_eth) # NO sig 2-way interaction

mod_likeme_dv_pgen<-lmer(resp~stim*Kid_Gender + (1|SID),data=rage_dv_likeme)
summary(mod_likeme_dv_pgen) # YES - interaction between self vs. other and kid gender

mod_likeme_dv_pgen_eth<-lmer(resp~stim*Kid_Gender*Kid_Eth + (1|SID),data=rage_dv_likeme)
summary(mod_likeme_dv_pgen_eth) #no 3 way interaction for like me x kid gender x kid_eth

mean_likeme_kidgen <- summarySEwithin(rage_dv_likeme, measurevar="resp", 
                                      withinvars="stim", 
                                      betweenvars = "Kid_Gender",
                                      idvar="SID", na.rm=TRUE, conf.interval=.95)
mean_likeme_kidgen$Kid_Gender<-revalue(mean_likeme_kidgen$Kid_Gender,
                                       c("-0.5"="Boy","0.5"="Girl"))
mean_likeme_kidgen #girls are faster to categorize exact matches; boys are slower to categorize exact matches
emm_likeme_pgen<-emmeans(mod_likeme_dv_pgen, "stim", by="Kid_Gender") #like me vs. not comparisons by kid's gender
pairs(emm_likeme_pgen) #girls are significantly faster to identify "like me" faces -- boys are not

plot_likeme_pgen <-ggplot() +
  geom_point(data = mean_likeme_kidgen, 
             aes(x = Kid_Gender, y = resp, colour = stim,shape=stim), 
             stat = "identity", size = 8) +
  geom_errorbar(data = mean_likeme_kidgen, width = .1, 
                aes(x = factor(Kid_Gender), ymin = resp-ci, ymax = resp+ci, colour = factor(stim))) +
  ylim(950,1150) +
  scale_x_discrete(labels=c("Boy","Girl"))+
  theme_few() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_color_economist() +
  labs(y = "Categorization Speed", x = "Child's Gender", colour ="Stimulus",shape="Stimulus") 

plot_likeme_pgen